#include "geef.h"
#include "oid.h"
#include "index.h"
#include "object.h"
#include <string.h>
#include <git2.h>

void geef_index_free(ErlNifEnv *env, void *cd)
{
	geef_index *index = (geef_index *) cd;
	git_index_free(index->index);
}

ERL_NIF_TERM
geef_index_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;
	ERL_NIF_TERM term;

	index = enif_alloc_resource(geef_index_type, sizeof(geef_index));
	if (!index)
		return atoms.error;

	if (git_index_new(&index->index) < 0)
		return geef_error(env);

	term = enif_make_resource(env, index);
	enif_release_resource(index);

	return enif_make_tuple2(env, atoms.ok, term);
}

ERL_NIF_TERM
geef_index_write(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	if (git_index_write(index->index) < 0)
		return geef_error(env);

	return atoms.ok;
}

ERL_NIF_TERM
geef_index_write_tree(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;
	geef_repository *repo;
	ErlNifBinary bin;
	git_oid id;
	int error;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	if (argc == 2) {
		if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
			return enif_make_badarg(env);

		error = git_index_write_tree_to(&id, index->index, repo->repo);
	} else {
		error = git_index_write_tree(&id, index->index);
	}

	if (error < 0)
		return geef_error(env);

	if (geef_oid_bin(&bin, &id) < 0)
		return atoms.error;

	return enif_make_tuple2(env, atoms.ok, enif_make_binary(env, &bin));
}

ERL_NIF_TERM
geef_index_read_tree(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;
	geef_object *tree;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	if (!enif_get_resource(env, argv[0], geef_object_type, (void **) &tree))
		return enif_make_badarg(env);

	if (git_index_read_tree(index->index, (git_tree *)tree->obj) < 0)
		return geef_error(env);

	return atoms.ok;
}

ERL_NIF_TERM
geef_index_nth(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;
	unsigned int n;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	if (!enif_get_uint(env, argv[1], &n))
		return enif_make_badarg(env);
}

ERL_NIF_TERM
geef_index_clear(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_index *index;

	if (!enif_get_resource(env, argv[0], geef_index_type, (void **) &index))
		return enif_make_badarg(env);

	git_index_clear(index->index);

	return atoms.ok;
}
