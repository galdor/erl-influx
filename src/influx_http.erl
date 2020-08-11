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

-module(influx_http).

-export([send_request/3]).

-export_type([conn/0, stream/0, method/0, status/0, header/0, request/0]).

-type conn() :: pid().
-type stream() :: reference().

-type method() :: get | post.
-type status() :: pos_integer().
-type header() :: [{binary(), binary()}].

-type request() :: #{method := method(),
                     path := binary(),
                     query => [{binary(), iodata()}],
                     body => iodata(),
                     api_token => binary(),
                     accept => binary(),
                     content_type => binary()}.

-type response() :: {status(), header(), Body :: binary()}.

-spec send_request(conn(), request(), pid()) -> pid().
send_request(Conn, Request, ReplyTo) ->
  spawn_link(fun () ->
                 case send_request_(Conn, Request) of
                   {ok, Response} ->
                     ReplyTo ! {request_success, Response};
                   {error, Reason} ->
                     ReplyTo ! {request_error, Reason}
                 end
             end).

-spec send_request_(conn(), request()) -> {ok, response()} | {error, term()}.
send_request_(Conn, Request) ->
  #{method := Method, path := Path} = Request,
  Query = maps:get(query, Request, []),
  QueryString = uri_string:compose_query(Query),
  FullPath = [Path, $?, QueryString],
  RequestHeader = request_header(Request),
  RequestBody = maps:get(body, Request, <<>>),
  Options = #{},
  Stream = gun:request(Conn, method_string(Method), FullPath,
                       RequestHeader, RequestBody, Options),
  case gun:await(Conn, Stream) of
    {response, fin, Status, Header} ->
      {ok, {Status, Header, <<>>}};
    {response, nofin, Status, Header} ->
      case gun:await_body(Conn, Stream) of
        {ok, Body} ->
          {ok, {Status, Header, Body}};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec request_header(request()) -> header().
request_header(Request) ->
  Fun = fun (K, V, Acc) ->
            case K of
              api_token ->
                [{<<"Authorization">>, [<<"Token ">>, V]} | Acc];
              accept ->
                [{<<"Accept">>, V} | Acc];
              content_type ->
                [{<<"Content-Type">>, V} | Acc];
              _ ->
                Acc
            end
        end,
  maps:fold(Fun, [], Request).

-spec method_string(method()) -> binary().
method_string(get) ->
  <<"GET">>;
method_string(post) ->
  <<"POST">>.
