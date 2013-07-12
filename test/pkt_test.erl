-module(pkt_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("src/geef_records.hrl").

flush_test() ->
    Expected = flush,
    Actual = geef_pkt:parse("0000"),
    ?assertMatch({Expected, _}, Actual).

done_test() ->
    Expected = done,
    Actual = geef_pkt:parse("0009done\n"),
    ?assertMatch({Expected, _}, Actual).

want_test() ->
    Expected = {want, geef_oid:parse("e17ca7f2d877acbf8b9a9a1cb4c243ca72e86463")},
    Actual = geef_pkt:parse("0032want e17ca7f2d877acbf8b9a9a1cb4c243ca72e86463\n"),
    ?assertMatch({Expected, _}, Actual).

wants_test() ->
    Data = <<"0032want e17ca7f2d877acbf8b9a9a1cb4c243ca72e86463\n0032want e17ca7f2d877acbf8b9a9a1cb4c243ca72e86464\n">>,
    Expected1 = {want, geef_oid:parse("e17ca7f2d877acbf8b9a9a1cb4c243ca72e86463")},
    Expected2 = {want, geef_oid:parse("e17ca7f2d877acbf8b9a9a1cb4c243ca72e86464")},
    {Actual1, Cont} = geef_pkt:parse(Data),
    ?assertMatch(Expected1, Actual1),
    {Actual2, _} = Cont([]),
    ?assertMatch(Expected2, Actual2).

request_test() ->
    Line = <<"0039git-upload-pack /schacon/gitbook.git\0host=example.com\0">>,
    Expected = #geef_request{service=upload_pack, path= <<"/schacon/gitbook.git">>, host= <<"example.com">>},
    {ok, Actual} = geef_pkt:parse_request(Line),
    ?assertEqual(Expected, Actual).

short_test() ->
    Actual = geef_pkt:parse("0032want e17ca7f2d877acbf8b9a"),
    ?assertMatch({continue, _}, Actual).

short_request_test() ->
    Line = <<"0039git-upload">>,
    Actual1 = geef_pkt:parse_request(Line),
    ?assertMatch({continue, _}, Actual1),
    {_, Cont} = Actual1,
    {ok, Actual2} = Cont(<<"-pack /schacon/gitbook.git\0host=example.com\0">>),
    Expected = #geef_request{service=upload_pack, path= <<"/schacon/gitbook.git">>, host= <<"example.com">>},
    ?assertEqual(Expected, Actual2).
