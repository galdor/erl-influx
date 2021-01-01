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

-module(influx_line_protocol).

-export([encode_point/1, encode_points/1,
         encode_measurement/1,
         encode_fields/1, encode_field/2, encode_field_value/1,
         encode_tags/1, encode_tag/2, encode_key/1,
         encode_string/1,
         encode_timestamp/1]).

-spec encode_point(influx:point()) -> iodata().
encode_point(Point) ->
  Measurement = maps:get(measurement, Point),
  Fields = maps:get(fields, Point),
  TagsData = case maps:find(tags, Point) of
               {ok, Tags} when map_size(Tags) > 0 ->
                 [$,, encode_tags(Tags)];
               _ ->
                 []
             end,
  TimestampData = case maps:find(timestamp, Point) of
                    {ok, Value} ->
                      [$ , encode_timestamp(Value)];
                    error ->
                      []
                  end,
  [encode_measurement(Measurement),
   TagsData,
   $ , encode_fields(Fields),
   TimestampData].

-spec encode_points([influx:point()]) -> iodata().
encode_points(Points) ->
  [[influx_line_protocol:encode_point(Point), $\n] || Point <- Points].

-spec encode_measurement(influx:key()) -> binary().
encode_measurement(Measurement) when is_atom(Measurement) ->
  encode_measurement(atom_to_binary(Measurement), <<>>);
encode_measurement(Measurement) ->
  encode_measurement(unicode:characters_to_binary(Measurement), <<>>).

encode_measurement(<<>>, Acc) ->
  Acc;
encode_measurement(<<Char, Rest/binary>>, Acc) ->
  case Char of
    C when C == $,; C == $=; C == $  ->
      encode_measurement(Rest, <<Acc/binary, $\\, C>>);
    C ->
      encode_measurement(Rest, <<Acc/binary, C>>)
  end.

-spec encode_fields(influx:fields()) -> iodata().
encode_fields(Fields) when map_size(Fields) == 0 ->
  error(empty_fields);
encode_fields(Fields) ->
  Entries = lists:sort(fun ({K1, _}, {K2, _}) ->
                           K1 =< K2
                       end, maps:to_list(Fields)),
  Data = lists:map(fun ({K, V}) -> encode_field(K, V) end, Entries),
  lists:join($,, Data).

-spec encode_field(influx:key(), influx:field_value()) -> iodata().
encode_field(Name, Value) ->
  [encode_key(Name), $=, encode_field_value(Value)].

-spec encode_field_value(influx:field_value()) -> iodata().
encode_field_value(V) when is_float(V) ->
  io_lib_format:fwrite_g(V);
encode_field_value(V) when is_integer(V) ->
  [integer_to_binary(V), $i];
encode_field_value(true) ->
  <<"true">>;
encode_field_value(false) ->
  <<"false">>;
encode_field_value(V) when is_binary(V) ->
  encode_string(V).

-spec encode_tags(influx:tags()) -> iodata().
encode_tags(Tags) ->
  Entries = lists:sort(fun ({K1, _}, {K2, _}) ->
                           K1 =< K2
                       end, maps:to_list(Tags)),
  Data = lists:map(fun ({K, V}) -> encode_tag(K, V) end, Entries),
  lists:join($,, Data).

-spec encode_tag(influx:key(), influx:key()) -> iodata().
encode_tag(Name, Value) ->
  [encode_key(Name), $=, encode_key(Value)].

-spec encode_key(influx:key()) -> binary().
encode_key(Key) when is_atom(Key) ->
  encode_key(atom_to_binary(Key), <<>>);
encode_key(Key) ->
  encode_key(unicode:characters_to_binary(Key), <<>>).

encode_key(<<>>, Acc) ->
  Acc;
encode_key(<<Char, Rest/binary>>, Acc) ->
  case Char of
    C when C == $,; C == $=; C == $  ->
      encode_key(Rest, <<Acc/binary, $\\, C>>);
    C ->
      encode_key(Rest, <<Acc/binary, C>>)
  end.

-spec encode_string(binary()) -> iodata().
encode_string(Bin) ->
  encode_string(Bin, <<>>).

encode_string(<<>>, Acc) ->
  [$", Acc, $"];
encode_string(<<$", Rest/binary>>, Acc) ->
  encode_string(Rest, <<Acc/binary, $\\, $">>);
encode_string(<<$\\, Rest/binary>>, Acc) ->
  encode_string(Rest, <<Acc/binary, $\\, $\\>>);
encode_string(<<C, Rest/binary>>, Acc) ->
  encode_string(Rest, <<Acc/binary, C>>).

-spec encode_timestamp(influx:timestamp()) -> iodata().
encode_timestamp(Value) ->
  integer_to_binary(Value).
