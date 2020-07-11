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

-module(influx_line_protocol_test).

-include_lib("eunit/include/eunit.hrl").

encode_point_test() ->
  Encode = fun (P) ->
               iolist_to_binary(influx_line_protocol:encode_point(P))
           end,
  ?assertEqual(<<"m1 a=1i">>,
               Encode(influx:point(<<"m1">>, #{<<"a">> => 1}))),
  ?assertEqual(<<"m2,x=1,y=2 a=1i,b=true">>,
               Encode(influx:point(<<"m2">>, #{<<"a">> => 1, <<"b">> => true},
                                   #{<<"x">> => <<"1">>, <<"y">> => <<"2">>}))),
  ?assertEqual(<<"m3 a=1i,b=true">>,
               Encode(influx:point(<<"m3">>, #{<<"a">> => 1, <<"b">> => true},
                                   #{}))),
  ?assertEqual(<<"m4,x=1,y=2 a=1i,b=true 1594156460484">>,
               Encode(influx:point(<<"m4">>, #{<<"a">> => 1, <<"b">> => true},
                                   #{<<"x">> => <<"1">>, <<"y">> => <<"2">>},
                                   1594156460484))),
  ?assertEqual(<<"m5 a=1i,b=true 1594156460484">>,
               Encode(influx:point(<<"m5">>, #{<<"a">> => 1, <<"b">> => true},
                                   #{}, 1594156460484))).

encode_measurement_test() ->
  Encode = fun (M) ->
               iolist_to_binary(influx_line_protocol:encode_measurement(M))
           end,
  ?assertEqual(<<"foo">>, Encode(<<"foo">>)),
  ?assertEqual(<<"\\,foo\\,\\ bar\\ ">>, Encode(<<",foo, bar ">>)).

encode_fields_test() ->
  Encode = fun (Fields) ->
               iolist_to_binary(influx_line_protocol:encode_fields(Fields))
           end,
  ?assertError(empty_fields, Encode(#{})),
  ?assertEqual(<<"foo=-1i">>, Encode(#{<<"foo">> => -1})),
  ?assertEqual(<<"a=\"foo\",b1=false,b2=true,c=42i">>,
               Encode(#{<<"c">> => 42,
                        <<"b1">> => false,
                        <<"a">> => <<"foo">>,
                        <<"b2">> => true})).

encode_field_test() ->
  Encode = fun (K, V) ->
               iolist_to_binary(influx_line_protocol:encode_field(K, V))
           end,
  ?assertEqual(<<"f1=1.0">>, Encode(<<"f1">>, 1.0)),
  ?assertEqual(<<"f2=1.0e78">>, Encode(<<"f2">>, 1.0e+78)),
  ?assertEqual(<<"f3=-3.14e-78">>, Encode(<<"f3">>, -3.14e-78)),
  ?assertEqual(<<"i1=-1i">>, Encode(<<"i1">>, -1)),
  ?assertEqual(<<"i2=42i">>, Encode(<<"i2">>, 42)),
  ?assertEqual(<<"b1=true">>, Encode(<<"b1">>, true)),
  ?assertEqual(<<"b2=false">>, Encode(<<"b2">>, false)),
  ?assertEqual(<<"s1=\"\"">>, Encode(<<"s1">>, <<"">>)),
  ?assertEqual(<<"s2=\"bar\"">>, Encode(<<"s2">>, <<"bar">>)),
  ?assertEqual(<<"a\\,b\\=c\\ =\"hello\"">>, Encode(<<"a,b=c ">>, <<"hello">>)).

encode_tags_test() ->
  Encode = fun (Tags) ->
               iolist_to_binary(influx_line_protocol:encode_tags(Tags))
           end,
  ?assertEqual(<<"foo=bar">>, Encode(#{<<"foo">> => <<"bar">>})),
  ?assertEqual(<<"a=hello,b1=bar,b2=world,c=foo">>,
               Encode(#{<<"c">> => <<"foo">>,
                        <<"b1">> => <<"bar">>,
                        <<"a">> => <<"hello">>,
                        <<"b2">> => <<"world">>})).

encode_tag_test() ->
  Encode = fun (K, V) ->
               iolist_to_binary(influx_line_protocol:encode_tag(K, V))
           end,
  ?assertEqual(<<"tag1=bar">>, Encode(<<"tag1">>, <<"bar">>)),
  ?assertEqual(<<"tag2=\"bar\"">>, Encode(<<"tag2">>, <<"\"bar\"">>)),
  ?assertEqual(<<"tag3=a\\ b\\ c">>, Encode(<<"tag3">>, <<"a b c">>)),
  ?assertEqual(<<"tag4=a\\=b">>, Encode(<<"tag4">>, <<"a=b">>)),
  ?assertEqual(<<"a\\,b\\=c\\ =\\ ">>, Encode(<<"a,b=c ">>, <<" ">>)).

encode_key_test() ->
  Encode = fun (K) ->
               iolist_to_binary(influx_line_protocol:encode_key(K))
           end,
  ?assertEqual(<<"foo">>, Encode(foo)),
  ?assertEqual(<<"foo">>, Encode("foo")),
  ?assertEqual(<<"foo">>, Encode(<<"foo">>)),
  ?assertEqual(<<"a\\,b\\=\\ c">>, Encode("a,b= c")),
  ?assertEqual(<<"été"/utf8>>, Encode('été')),
  ?assertEqual(<<"été"/utf8>>, Encode("été")),
  ?assertEqual(<<"été"/utf8>>, Encode(<<"été"/utf8>>)).

encode_string_test() ->
  Encode = fun (Bin) ->
               iolist_to_binary(influx_line_protocol:encode_string(Bin))
           end,
  ?assertEqual(<<"\"\"">>, Encode(<<"">>)),
  ?assertEqual(<<"\"foo\"">>, Encode(<<"foo">>)),
  ?assertEqual(<<"\"foo bar\"">>, Encode(<<"foo bar">>)),
  ?assertEqual(<<"\" \"">>, Encode(<<" ">>)),
  ?assertEqual(<<"\"foo \\\"bar\\\" baz\"">>, Encode(<<"foo \"bar\" baz">>)),
  ?assertEqual(<<"\"\\\"foo\\\"\"">>, Encode(<<"\"foo\"">>)),
  ?assertEqual(<<"\"\\\\\\\\\"">>, Encode(<<"\\\\">>)).

encode_timestamp_test() ->
  Encode = fun (T) ->
               iolist_to_binary(influx_line_protocol:encode_timestamp(T))
           end,
  ?assertEqual(<<"1594156460">>, Encode(1594156460)),
  ?assertEqual(<<"1594156460484">>, Encode(1594156460484)),
  ?assertEqual(<<"1594156460484120">>, Encode(1594156460484120)),
  ?assertEqual(<<"1594156460484120883">>, Encode(1594156460484120883)).
