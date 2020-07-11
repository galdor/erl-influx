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

-module(influx_statistics_probe).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-spec start_link() -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  timer:send_interval(1000, collect_measurements),
  {ok, undefined}.

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(collect_measurements, State) ->
  influx_client:enqueue_point(influx_client, statistics_point()),
  {noreply, State};

handle_info(_Msg, State) ->
  {noreply, State}.

statistics_point() ->
  {ContextSwitches, _} = statistics(context_switches),
  {NbGCs, WordsReclaimed, _} = statistics(garbage_collection),
  {{input, Input}, {output, Output}} = statistics(io),
  {Reductions, _} = statistics(reductions),
  {Runtime, _} = statistics(runtime),
  {WallClock, _} = statistics(wall_clock),
  TotalActiveTasks = statistics(total_active_tasks),
  TotalActiveTasksAll = statistics(total_active_tasks_all),
  TotalRunQueueLengths = statistics(total_run_queue_lengths),
  TotalRunQueueLengthsAll = statistics(total_run_queue_lengths_all),
  Data = #{context_switches => ContextSwitches,
           nb_gcs => NbGCs,
           words_reclaimed => WordsReclaimed,
           input => Input,
           output => Output,
           reductions => Reductions,
           runtime => Runtime,
           total_active_tasks => TotalActiveTasks,
           total_active_tasks_all => TotalActiveTasksAll,
           total_run_queue_lengths => TotalRunQueueLengths,
           total_run_queue_lengths_all => TotalRunQueueLengthsAll,
           wall_clock => WallClock},
  influx:point(erlang_statistics, Data).
