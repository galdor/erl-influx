%% Copyright (c) 2020-2021 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(influx_client).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([process_name/1, start_link/2, enqueue_point/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([name/0, ref/0, options/0]).

-type name() :: c_gen_server:name().
-type ref() :: c_gen_server:ref().

-type options() :: #{uri => binary(),
                     mhttp_pool => mhttp:pool_id(),
                     max_queue_length => pos_integer(),
                     send_interval => pos_integer(),
                     bucket => binary(),
                     org => binary(),
                     precision => influx:precision(),
                     tags => influx:tags()}.

-type state() :: #{options := options(),
                   uri := uri:uri(),
                   backoff := backoff:backoff(),
                   queue := [influx:point()],
                   queue_length := non_neg_integer()}.

-spec process_name(influx:client_id()) -> atom().
process_name(Id) ->
  Name = <<"influx_client_", (atom_to_binary(Id))/binary>>,
  binary_to_atom(Name).

-spec start_link(influx:client_id(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Id, Options) ->
  Name = process_name(Id),
  gen_server:start_link({local, Name}, ?MODULE, [Options], []).

-spec enqueue_point(influx:client_id(), influx:point()) -> ok | {error, term()}.
enqueue_point(Id, Point) ->
  Name = process_name(Id),
  gen_server:call(Name, {enqueue_point, Point}).

init([Options]) ->
  logger:update_process_metadata(#{domain => [influx, client]}),
  URIString = maps:get(uri, Options, <<"http://localhost:8086">>),
  case uri:parse(URIString) of
    {ok, URI} ->
      Interval = maps:get(send_interval, Options, 2500),
      Backoff = backoff:type(backoff:init(Interval, 60000), jitter),
      State = #{options => Options,
                uri => URI,
                backoff => Backoff,
                queue => [],
                queue_length => 0},
      schedule_next_send(State),
      {ok, State};
    {error, Reason} ->
      {stop, {invalid_uri, Reason}}
  end.

handle_call({enqueue_point, Point}, _From,
            State = #{options := Options,
                      queue := Queue,
                      queue_length := QueueLength}) ->
  MaxQueueLength = maps:get(max_queue_length, Options, 10_000),
  if
    QueueLength >= MaxQueueLength ->
      {reply, {error, queue_full}, State};
    true ->
      ClientTags = maps:get(tags, Options, #{}),
      PointTags = maps:get(tags, Point, #{}),
      Point2 = Point#{tags => maps:merge(ClientTags, PointTags)},
      State2 = State#{queue => [Point2 | Queue],
                      queue_length => QueueLength + 1},
      {reply, ok, State2}
  end;

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p~n", [Msg, From]),
  {reply, unhandled, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p~n", [Msg]),
  {noreply, State}.

handle_info(send_points,
            State = #{options := Options,
                      uri := URI0,
                      backoff := Backoff,
                      queue := Queue,
                      queue_length := QueueLength}) when
    QueueLength > 0 ->
  URIRef = #{path => <<"/api/v2/write">>,
             query => request_query(Options)},
  URI = uri:resolve_reference(URIRef, URI0),
  Req = #{method => post,
          target => URI,
          header => request_header(Options),
          body => influx_line_protocol:encode_points(Queue)},
  Pool = maps:get(mhttp_pool, Options, default),
  case mhttp:send_request(Req, #{pool => Pool}) of
    {ok, #{status := Status}} when Status >= 200; Status < 300 ->
      {_, Backoff2} = backoff:succeed(Backoff),
      State2 = State#{backoff => Backoff2,
                      queue => [], queue_length => 0},
      schedule_next_send(State2),
      {noreply, State2};
    {ok, Res = #{status := Status}} ->
      ?LOG_ERROR("cannot send points: request failed with status ~b (~p)",
                 [Status, mhttp_response:body(Res)]),
      {_, Backoff2} = backoff:fail(Backoff),
      State2 = State#{backoff => Backoff2},
      schedule_next_send(State2),
      {noreply, State2};
    {error, Reason} ->
      ?LOG_ERROR("cannot send points: ~p", [Reason]),
      {_, Backoff2} = backoff:fail(Backoff),
      State2 = State#{backoff => Backoff2},
      schedule_next_send(State2),
      {noreply, State2}
  end;
handle_info(send_points, State) ->
  schedule_next_send(State),
  {noreply, State};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p~n", [Msg]),
  {noreply, State}.

-spec schedule_next_send(state()) -> ok.
schedule_next_send(#{backoff := Backoff}) ->
  erlang:send_after(backoff:get(Backoff), self(), send_points),
  ok.

-spec request_header(options()) -> mhttp:header().
request_header(Request) ->
  Fun = fun (K, V, Acc) ->
            case K of
              api_token ->
                [{<<"Authorization">>, [<<"Token ">>, V]} | Acc];
              _ ->
                Acc
            end
        end,
  maps:fold(Fun, [], Request).

-spec request_query(options()) -> uri:query().
request_query(Options) ->
  F = fun
        (bucket, V, Acc) ->
          [{<<"bucket">>, V} | Acc];
        (org, V, Acc) ->
          [{<<"org">>, V} | Acc];
        (precision, V, Acc) ->
          [{<<"precision">>, precision_query_parameter(V)} | Acc];
        (_, _, Acc) ->
          Acc
      end,
  maps:fold(F, [], Options).

-spec precision_query_parameter(influx:precision()) -> binary().
precision_query_parameter(second) ->
  <<"s">>;
precision_query_parameter(millisecond) ->
  <<"ms">>;
precision_query_parameter(microsecond) ->
  <<"us">>;
precision_query_parameter(nanosecond) ->
  <<"ns">>.
