-record(geef_reference, {name :: binary(), type :: atom(), target :: binary() | geef_oid()}).
-record(geef_oid, {oid}).
-record(geef_object, {type :: atom(), id :: geef_oid(), handle}).
-record(geef_index_entry,
	{ctime :: non_neg_integer(), mtime :: non_neg_integer(),
	 dev :: non_neg_integer(), ino :: non_neg_integer(),
	 mode :: non_neg_integer(), uid :: non_neg_integer(), gid :: non_neg_integer(),
	 size :: non_neg_integer(),
	 id :: geef_oid(),
	 flags, flags_extended,
	 path :: iolist()}).

-record(geef_request, {service :: atom, path :: binary(), host :: binary()}).
-record(geef_tree_entry, {mode, type, id, name}).
-record(geef_signature, {name :: iolist(), email :: iolist(), time :: geef_time()}).

-type geef_reference() :: #geef_reference{}.
-type geef_oid() :: #geef_oid{}.
-type geef_object() :: #geef_object{}.
-type geef_index_entry() :: #geef_index_entry{}.
-type geef_request() :: #geef_request{}.
-type geef_tree_entry() :: #geef_tree_entry{}.
-type geef_signature() :: #geef_signature{}.
-type geef_time() :: {erlang:timestamp(), non_neg_integer()}.
