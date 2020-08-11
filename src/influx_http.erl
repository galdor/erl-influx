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

-export_type([conn/0, stream/0, method/0, status/0, header/0, req/0]).

-type conn() :: pid().
-type stream() :: reference().

-type method() :: get | post.
-type status() :: pos_integer().
-type header() :: [{binary(), binary()}].

-type req() :: #{method := method(),
                 path := binary(),
                 query => [{binary(), iodata()}],
                 body => iodata(),
                 api_token => binary(),
                 accept => binary(),
                 content_type => binary()}.

-type res() :: {status(), header(), Body :: binary()}.

-spec send_request(conn(), req(), pid()) -> pid().
send_request(Conn, Req, ReplyTo) ->
  spawn_link(fun () ->
                 case send_request_(Conn, Req) of
                   {ok, Response} ->
                     ReplyTo ! {request_success, Response};
                   {error, Reason} ->
                     ReplyTo ! {request_error, Reason}
                 end
             end).

-spec send_request_(conn(), req()) -> {ok, res()} | {error, term()}.
send_request_(Conn, Req) ->
  #{method := Method, path := Path} = Req,
  Query = maps:get(query, Req, []),
  QueryString = uri_string:compose_query(Query),
  FullPath = [Path, $?, QueryString],
  ReqHeader = request_header(Req),
  ReqBody = maps:get(body, Req, <<>>),
  Options = #{},
  Stream = gun:request(Conn, method_string(Method), FullPath,
                       ReqHeader, ReqBody, Options),
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

-spec request_header(req()) -> header().
request_header(Req) ->
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
  maps:fold(Fun, [], Req).

-spec method_string(method()) -> binary().
method_string(get) ->
  <<"GET">>;
method_string(post) ->
  <<"POST">>.
