-module(geef_commit).
-export([tree_id/1, tree/1, lookup/2]).
-export([create/5, create/6, create/7]).

-include("geef_records.hrl").

-type commit() :: geef_obj:object(commit).
-export_type([commit/0]).

-spec tree_id(commit()) -> geef_oid:oid().
tree_id(#geef_object{type=commit,handle=Handle}) ->
    geef_nif:commit_tree_id(Handle).

-spec tree(commit()) -> {ok, geef_tree:tree()} | {error, term()}.
tree(#geef_object{type=commit,handle=Handle}) ->
    case geef_nif:commit_tree(Handle) of
	{ok, Type, Handle} ->
	    {ok, #geef_object{type=Type, handle=Handle}};
	Other ->
	    Other
    end.

-spec lookup(pid(), geef_oid:oid()) -> {ok, commit()} | {error, term()}.
lookup(Repo, Id) ->
    geef_obj:lookup(Repo, Id, commit).


%% Full version, accepts all paremeters
-spec create(pid(), iolist(), geef_sig:signature(), geef_sig:signature(),
	     iolist(), iolist(), geef_oid:oid(), [geef_oid:oid()]) -> {ok, geef_oid:oid()} | {error, term()}.
create(Repo, Ref, Author = #geef_signature{}, Committer = #geef_signature{}, Encoding, Message, Tree, Parents)
  when is_list(Parents) ->
    Handle = geef_repo:handle(Repo),
    geef_nif:commit_create(Handle, Ref, Author, Committer, Encoding, Message, Tree, Parents).

% Common version, accepts ref and encoding as options
create(Repo, Author = #geef_signature{}, Committer = #geef_signature{}, Message, Tree, Parents, Opts) ->
    Ref = proplists:get_value(update_ref, Opts, undefined),
    Encoding = proplists:get_value(encoding, Opts, undefined),
    create(Repo, Ref, Author, Committer, Encoding, Message, Tree, Parents).

create(Repo, Author = #geef_signature{}, Committer = #geef_signature{}, Message, Tree, Parents) ->
    create(Repo, Author, Committer, Message, Tree, Parents, []);

% Version with both the same
%% @doc Create a new commit. Person will be used for both author and commiter.
create(Repo, Person = #geef_signature{}, Message, Tree, Parents, Opts) ->
    create(Repo, Person, Person, Message, Tree, Parents, Opts).

create(Repo, Person = #geef_signature{}, Message, Tree, Parents) ->
    create(Repo, Person, Person, Message, Tree, Parents, []).
