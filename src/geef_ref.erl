-module(geef_ref).

-export([lookup/2, resolve/2, create/4, dwim/2, shorthand/1]).

-include("geef_records.hrl").

-spec new({binary(), oid | symbolic, binary()}) -> geef_reference().
new({Name, oid, Target}) ->
    #geef_reference{name = Name, type = oid, target = #geef_oid{oid = Target}};
new({Name, symbolic, Target}) ->
    #geef_reference{name = Name, type = symbolic, target = Target}.

-spec create(pid(), iolist(), geef_oid() | binary(), boolean()) -> {ok, geef_reference()} | {error, term()}.
create(Repo, Refname, Target, Force) ->
    case geef_repo:create_reference(Repo, Refname, Target, Force) of
        {ok, Spec} ->
            {ok, new(Spec)};
        Error ->
            Error
    end.

-spec lookup(pid(), iolist()) -> {ok, geef_reference()} | {error, term()}.
lookup(Repo, Refname) ->
    case geef_repo:lookup_reference(Repo, Refname) of
	{ok, Spec} ->
	    {ok, new(Spec)};
	Other ->
	    Other
    end.

-spec resolve(pid(), geef_reference()) -> {ok, geef_reference()} | {error, term()}.
resolve(_Repo, Ref = #geef_reference{type=oid}) ->
    {ok, Ref}; % resolving an oid ref is a no-op, skip going into the NIF
resolve(Repo, #geef_reference{name=Name}) ->
    case geef_repo:lookup_resolved(Repo, Name) of
	{ok, Spec} ->
	    {ok, new(Spec)};
	Other ->
	    Other
    end.

-spec dwim(pid(), iolist()) -> {ok, geef_reference()} | {error, term()}.
dwim(Repo, Name) ->
    case geef_repo:reference_dwim(Repo, Name) of
	{ok, Spec} ->
	    {ok, new(Spec)};
	Err ->
	    Err
    end.

%% @doc Get the shorthand name for a particular reference
-spec shorthand(geef_reference() | binary()) -> binary().
shorthand(<<"refs/heads/", Rest/binary>>) ->
    Rest;
shorthand(<<"refs/tags/", Rest/binary>>) ->
    Rest;
shorthand(<<"refs/remotess/", Rest/binary>>) ->
    Rest;
shorthand(<<"refs/", Rest/binary>>) ->
    Rest;
shorthand(#geef_reference{name=Name}) ->
    shorthand(iolist_to_binary(Name)).
