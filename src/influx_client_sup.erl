%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
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

-module(influx_client_sup).

-behaviour(supervisor).

-export([start_link/0, start_client/2]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_client(influx:client_id(), influx_client:options()) ->
        supervisor:startchild_ret().
start_client(Id, Options) ->
  supervisor:start_child(?MODULE, client_child_spec(Id, Options)).

init([]) ->
  Children = client_child_specs(),
  {ok, {{one_for_one, 1, 5}, Children}}.

-spec client_child_specs() -> [supervisor:child_spec()].
client_child_specs() ->
  EnvClientSpecs = application:get_env(influx, clients, #{}),
  ClientSpecs = case maps:is_key(default, EnvClientSpecs) of
                  true ->
                    EnvClientSpecs;
                  false ->
                    EnvClientSpecs#{default => #{}}
                end,
  maps:fold(fun (Id, Options, Acc) ->
                [client_child_spec(Id, Options) | Acc]
            end,
            [], ClientSpecs).

-spec client_child_spec(influx:client_id(), influx_client:options()) ->
        supervisor:child_spec().
client_child_spec(Id, Options) ->
  #{id => Id,
    start => {influx_client, start_link, [Id, Options]}}.
