-module(repo_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("src/geef_records.hrl").

repo_test_() ->
    {foreach, fun start/0, fun stop/1, [fun bare_test/1, fun odb_write_test/1,
					fun ref_test/1, fun index_add_test/1,
					fun ref_iter_test/1, fun revparse_test/1,
					fun commit_create_test/1]}.

start() ->
    {A, B, C} = now(),
    N = node(),
    TmpDir = io_lib:format("/tmp/geef-~p~p~p~p.git", [N, A, B, C]),
    {ok, Repo} = geef_repo:init(TmpDir, true),
    Repo.

bare_test(Repo) ->
    [?_assertEqual(geef_repo:is_bare(Repo), true)].

odb_write_test(Repo) ->
    Data = <<"This is some text that will go in a file">>,
    {ok, Odb} = geef_repo:odb(Repo),
    {ok, Actual} = geef_odb:write(Odb, Data, blob),
    Expected = geef_oid:parse("c300118399f01fe52b316061b5d32beb27e0adfd"),
    [?_assertEqual(Actual, Expected)].

revparse_test(Repo) ->
    odb_write_test(Repo),
    {ok, Obj} = geef_revparse:single(Repo, "c300118399"),
    [?_assertEqual(Obj#geef_object.type, blob),
     ?_assertEqual(Obj#geef_object.id, geef_oid:parse("c300118399f01fe52b316061b5d32beb27e0adfd"))].

index_add_test(Repo) ->
    Data = <<"This is some text that will go in a file">>,
    {ok, Odb} = geef_repo:odb(Repo),
    {ok, BlobId} = geef_odb:write(Odb, Data, blob),
    {ok, Idx} = geef_index:new(),
    {NowMega, NowSecs, _} = os:timestamp(),
    Time = NowMega * 1000000 + NowSecs,
    Entry = #geef_index_entry{mode=8#100644, id=BlobId, path="README", size=size(Data), mtime=Time},
    ok = geef_index:add(Idx, Entry),
    {ok, TreeId} = geef_index:write_tree(Idx, Repo),
    Expected = geef_oid:parse("5a20bbbf65ea75ad4d9f995d179156824ccca3a1"),
    {ok, Entry1} = geef_index:get(Idx, "README", 0),
    [?_assertEqual(Expected, TreeId),
     ?_assertEqual(BlobId, Entry1#geef_index_entry.id),
     ?_assertEqual(size(Data), Entry1#geef_index_entry.size),
     ?_assertEqual(Time, Entry1#geef_index_entry.mtime),
     ?_assertEqual(geef_index:count(Idx), 1)].

ref_test(Repo) ->
    odb_write_test(Repo),
    Id = geef_oid:parse("c300118399f01fe52b316061b5d32beb27e0adfd"),
    {ok, _} = geef_ref:create(Repo, "refs/heads/branch", Id, true),
    {ok, _} = geef_ref:create_symbolic(Repo, "refs/heads/other", "refs/heads/branch", true),
    {ok, Ref0} = geef_ref:lookup(Repo, "refs/heads/branch"),
    {ok, Ref1} = geef_ref:lookup(Repo, "refs/heads/other"),
    {ok, Ref2} = geef_ref:resolve(Ref0),
    {ok, Dwimed} = geef_ref:dwim(Repo, "branch"),
    [?_assertEqual(Ref0#geef_reference.target, Id),
     ?_assertEqual(Ref1#geef_reference.target, <<"refs/heads/branch">>),
     ?_assertEqual(<<"branch">>, geef_ref:shorthand(Ref0)),
     ?_assertEqual(Ref2#geef_reference.target, Id),
     ?_assertEqual(Ref0, Dwimed)].

ref_iter_test(Repo) ->
    odb_write_test(Repo),
    Id = geef_oid:parse("c300118399f01fe52b316061b5d32beb27e0adfd"),
    {ok, _} = geef_ref:create(Repo, "refs/heads/branch", Id, true),
    {ok, _} = geef_ref:create_symbolic(Repo, "refs/heads/other", "refs/heads/branch", true),
    {ok, Iter0} = geef_ref:iterator(Repo),
    {ok, Ref0} = geef_ref:next(Iter0),
    {ok, Ref1} = geef_ref:next(Iter0),
    Res0 = geef_ref:next(Iter0),
    {ok, Iter1} = geef_ref:iterator(Repo, "refs/heads/b*"),
    {ok, Ref2} = geef_ref:next(Iter1),
    Res1 = geef_ref:next(Iter1),
    [?_assertEqual(Ref0#geef_reference.name, <<"refs/heads/branch">>),
     ?_assertEqual(Ref1#geef_reference.name, <<"refs/heads/other">>),
     ?_assertEqual(Res0, {error, iterover}),
     ?_assertEqual(Ref2#geef_reference.name, <<"refs/heads/branch">>),
     ?_assertEqual(Res1, {error, iterover})].

commit_create_test(Repo) ->
    odb_write_test(Repo),
    index_add_test(Repo),
    Tree = geef_oid:parse("5a20bbbf65ea75ad4d9f995d179156824ccca3a1"),
    Sig = #geef_signature{name= <<"foo">>, email= <<"bar">>, time={{1381,949139,0}, 120}},
    Message = <<"Commit message">>,
    Resp = geef_commit:create(Repo, Sig, Message, Tree, []),
    ?debugMsg(Resp),
    [?_assertMatch({ok, _}, Resp)].

rm_r(Path) ->
    case filelib:is_dir(Path) of
	false ->
	    file:delete(Path);
	true ->
	    {ok, Entries} = file:list_dir(Path),
	    lists:foreach(fun(X) -> ok = rm_r(filename:join([Path, X])) end, Entries),
	    file:del_dir(Path)
    end.

stop(Repo) ->
    Path = geef_repo:path(Repo),
    geef_repo:stop(Repo),
    ok = rm_r(Path).
