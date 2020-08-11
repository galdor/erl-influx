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

-module(influx).

-export([current_timestamp/0, current_timestamp/1,
         point/2, point/3, point/4]).

-export_type([timestamp/0, precision/0, key/0, field_value/0, fields/0, tags/0,
              point/0]).

-type timestamp() :: integer().

-type precision() :: second | millisecond | microsecond | nanosecond.

-type key() :: atom() | unicode:chardata().

-type field_value() :: float() | integer() | binary() | boolean().
-type fields() :: #{key() := field_value()}.

-type tags() :: #{key() := key()}.

-type point() :: #{measurement := key(),
                   fields := fields(),
                   tags => tags(),
                   timestamp => timestamp()}.

-spec current_timestamp() -> timestamp().
current_timestamp() ->
  current_timestamp(nanosecond).

-spec current_timestamp(precision()) -> timestamp().
current_timestamp(Unit) ->
  os:system_time(Unit).

-spec point(Measurement :: binary(), fields()) -> point().
point(Measurement, Fields) ->
  #{measurement => Measurement,
    fields => Fields}.

-spec point(Measurement :: binary(), fields(), tags()) -> point().
point(Measurement, Fields, Tags) ->
  #{measurement => Measurement,
    fields => Fields,
    tags => Tags}.

-spec point(Measurement :: binary(), fields(), tags(), timestamp()) -> point().
point(Measurement, Fields, Tags, Timestamp) ->
  #{measurement => Measurement,
    fields => Fields,
    tags => Tags,
    timestamp => Timestamp}.
