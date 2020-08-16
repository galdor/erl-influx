%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(influx_client).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1, start_link/2, enqueue_point/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([name/0, ref/0, options/0]).

-type name() :: {local, term()} | {global, term()} | {via, atom(), term()}.
-type ref() :: term() | {term(), atom()} | {global, term()}
             | {via, atom(), term()} | pid().

-type options() :: #{host => inet:hostname() | inet:ip_address(),
                     port => inet:port_number(),
                     tcp_options => [gen_tcp:connect_option()],
                     tls => boolean(),
                     tls_options => [ssl:tls_client_option()],
                     max_queue_length => pos_integer(),
                     send_interval => pos_integer(),
                     bucket => binary(),
                     org => binary(),
                     precision => influx:precision(),
                     tags => influx:tags()}.

-type state() :: #{options := options(),
                   queue := [influx:point()],
                   queue_length := non_neg_integer(),
                   conn := pid(),
                   connected := boolean(),
                   req => pid()}.

-spec start_link(name()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Name) ->
  start_link(Name, #{}).

-spec start_link(name(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Name, Options) ->
  gen_server:start_link(Name, ?MODULE, [Options], []).

-spec enqueue_point(ref(), influx:point()) -> ok | {error, term()}.
enqueue_point(Ref, Point) ->
  gen_server:call(Ref, {enqueue_point, Point}).

init([Options]) ->
  logger:update_process_metadata(#{domain => [influx, client]}),
  process_flag(trap_exit, true),
  ConnOptions = connect_options(Options),
  Host = maps:get(host, Options, "localhost"),
  Port = maps:get(port, Options, 8086),
  {ok, Conn} = gun:open(Host, Port, ConnOptions),
  State = #{options => Options,
            queue => [],
            queue_length => 0,
            conn => Conn,
            connected => false},
  schedule_next_send(State),
  {ok, State}.

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
  {noreply, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p~n", [Msg]),
  {noreply, State}.

handle_info(send_points,
            State = #{options := Options,
                      queue := Queue,
                      queue_length := QueueLength,
                      conn := Conn,
                      connected := Connected}) when
    Connected == true, QueueLength > 0 ->
  Req = #{method => post,
          path => <<"/api/v2/write">>,
          query => request_query_string(Options),
          body => influx_line_protocol:encode_points(Queue)},
  ReqPid = influx_http:send_request(Conn, Req, self()),
  State2 = State#{queue => [], queue_length => 0, req => ReqPid},
  {noreply, State2};
handle_info(send_points, State) ->
  schedule_next_send(State),
  {noreply, State};

handle_info({request_success, {Status, _, _}}, State) when
    Status >= 200, Status < 300 ->
  ?LOG_DEBUG("request succeeded status ~p: ~p~n", [Status]),
  {noreply, State};

handle_info({request_success, {Status, _, Body}}, State) ->
  ?LOG_ERROR("request failed with status ~p: ~p~n", [Status, Body]),
  {noreply, State};

handle_info({request_error, Reason}, State) ->
  ?LOG_ERROR("request failed: ~p~n", [Reason]),
  {noreply, State};

handle_info({'EXIT', Pid, Reason}, State = #{req := Pid}) ->
  case Reason of
    normal ->
      ok;
    _ ->
      ?LOG_ERROR("request process exited: ~p~n", [Reason])
  end,
  schedule_next_send(State),
  {noreply, maps:remove(req, State)};

handle_info({gun_up, _Conn, _Protocol}, State) ->
  ?LOG_INFO("connection established~n", []),
  {noreply, State#{connected => true}};

handle_info({gun_down, _Conn, _Protocol, Reason, _Streams}, State) ->
  ?LOG_INFO("connection lost: ~p~n", [Reason]),
  {noreply, State#{connected => false}};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p~n", [Msg]),
  {noreply, State}.

-spec schedule_next_send(state()) -> ok.
schedule_next_send(#{options := Options}) ->
  Interval = maps:get(send_interval, Options, 1000),
  erlang:send_after(Interval, self(), send_points),
  ok.

-spec connect_options(options()) -> gun:opts().
connect_options(Options) ->
  TCPOptions = maps:get(tcp_options, Options, []),
  TLSOptions = maps:get(tls_options, Options, []),
  Transport = case maps:get(tls, Options, false) of
                true -> tls;
                false -> tcp
              end,
  Retry = fun (_NbRetries, _Options) -> #{retries => 1, timeout => 5000} end,
  #{transport => Transport,
    tcp_opts => TCPOptions,
    tls_opts => TLSOptions,
    retry_fun => Retry}.

-spec request_query_string(options()) -> [{binary(), binary()}].
request_query_string(Options) ->
  Fun = fun (K, V, Acc) ->
            case K of
              bucket ->
                [{<<"bucket">>, V} | Acc];
              org ->
                [{<<"org">>, V} | Acc];
              precision ->
                [{<<"precision">>, precision_query_parameter(V)} | Acc];
              _ ->
                Acc
            end
        end,
  maps:fold(Fun, [], Options).

-spec precision_query_parameter(influx:precision()) -> binary().
precision_query_parameter(second) ->
  <<"s">>;
precision_query_parameter(millisecond) ->
  <<"ms">>;
precision_query_parameter(microsecond) ->
  <<"us">>;
precision_query_parameter(nanosecond) ->
  <<"ns">>.
