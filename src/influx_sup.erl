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

-module(influx_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Children = [client_child_spec() | probe_child_specs()],
  Flags = #{strategy => one_for_one,
            intensity => 1,
            period => 5},
  {ok, {Flags, Children}}.

-spec probes() -> [module()].
probes() ->
  [influx_memory_probe].

-spec probe_child_specs() -> [supervisor:child_spec()].
probe_child_specs() ->
  [#{id => Mod, start => {Mod, start_link, []}} || Mod <- probes()].

-spec client_child_spec() -> supervisor:child_spec().
client_child_spec() ->
  Name = influx_client,
  Opts = client_options(),
  #{id => influx_client,
    start => {Name, start_link, [{local, Name}, Opts]}}.

-spec client_options() -> influx_client:options().
client_options() ->
  Opts = application:get_env(influx, client, #{}),
  maps:merge(influx_client:default_options(), Opts).
