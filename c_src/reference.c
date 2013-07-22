#include "geef.h"
#include "repository.h"
#include "reference.h"
#include "oid.h"
#include <string.h>
#include <git2.h>

ERL_NIF_TERM
geef_reference_list(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	size_t i;
	git_strarray array;
	geef_repository *repo;
	ERL_NIF_TERM list;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (git_reference_list(&array, repo->repo) < 0)
		return geef_error(env);

	list = enif_make_list(env, 0);
	for (i = 0; i < array.count; i++) {
		ErlNifBinary bin;
		size_t len = strlen(array.strings[i]);

		if (!enif_alloc_binary(len, &bin))
			goto on_error;

		memcpy(bin.data, array.strings[i], len);
		list = enif_make_list_cell(env, enif_make_binary(env, &bin), list);
	}

	git_strarray_free(&array);

	return list;

on_error:
	git_strarray_free(&array);
	return geef_oom(env);
}

static ERL_NIF_TERM create_ref(ErlNifEnv *env, git_reference *ref)
{
	const char *name;
	ErlNifBinary name_bin, target_bin;
	size_t len;
	ERL_NIF_TERM type;

	/* Figure out the type */
	switch (git_reference_type(ref)) {
	case GIT_REF_OID:
		type = atoms.oid;
		break;
	case GIT_REF_SYMBOLIC:
		type = atoms.symbolic;
		break;
	default:
		type = atoms.error;
		break;
	}

	if (type == atoms.error)
		return atoms.error;

	if (git_reference_type(ref) == GIT_REF_OID) {
		const git_oid *id;
		id = git_reference_target(ref);

		if (geef_oid_bin(&target_bin, id) < 0)
			return geef_oom(env);
	} else {
		const char *name;
		size_t len;

		name = git_reference_symbolic_target(ref);
		len = strlen(name);

		if (enif_alloc_binary(len, &target_bin) < 0)
			return geef_oom(env);

		memcpy(target_bin.data, name, len);
	}

	/* Store the name */
	name = git_reference_name(ref);
	len = strlen(name);

	if (enif_alloc_binary(len, &name_bin) < 0)
		return geef_oom(env);

	memcpy(name_bin.data, name, len);

	return enif_make_tuple2(env, atoms.ok, enif_make_tuple3(env,
				enif_make_binary(env, &name_bin),
				type,
				enif_make_binary(env, &target_bin)));

}

ERL_NIF_TERM
geef_reference_lookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_repository *repo;
	git_reference *ref;
	ErlNifBinary bin;
	int error;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return enif_make_badarg(env);

	if (!geef_terminate_binary(&bin))
		return geef_oom(env);

	error = git_reference_lookup(&ref, repo->repo, (char *)bin.data);

	enif_release_binary(&bin);
	if (error < 0)
		return geef_error(env);

	return create_ref(env, ref);
}

ERL_NIF_TERM
geef_reference_lookup_resolved(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_ref *in_ref, *res_ref;
	ERL_NIF_TERM term_ref;
	git_reference *ref;
	git_repository *repo;
	ErlNifBinary bin;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return enif_make_badarg(env);

	if (!geef_terminate_binary(&bin))
		return geef_oom(env);

	if (git_reference_lookup_resolved(&ref, repo, (char *)bin.data))
		return geef_error(env);

	enif_release_binary(&bin);

	return create_ref(env, ref);
}

ERL_NIF_TERM
geef_reference_dwim(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary bin;
	geef_ref *ref;
	ERL_NIF_TERM term_ref;
	geef_repository *repo;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return enif_make_badarg(env);

	if (!geef_terminate_binary(&bin))
		return geef_oom(env);

	ref = enif_alloc_resource(geef_ref_type, sizeof(geef_ref));
	if (git_reference_dwim(&ref->ref, repo->repo, (char *)bin.data) < 0) {
		enif_release_binary(&bin);
		return geef_error(env);
	}

	term_ref = enif_make_resource(env, ref);
	enif_release_resource(ref);

	return enif_make_tuple2(env, atoms.ok, term_ref);
}

void geef_ref_free(ErlNifEnv *env, void *cd)
{
	geef_ref *ref = (geef_ref *) cd;
	git_reference_free(ref->ref);
}

struct list_data {
	ErlNifEnv *env;
	ERL_NIF_TERM list;
};

static int append_to_list(const char *name, void *payload)
{
	struct list_data *data = (struct list_data *) payload;
	ErlNifBinary bin;
	size_t len = strlen(name);

	if (!enif_alloc_binary(len, &bin))
		return -1;

	memcpy(bin.data, name, len);
	data->list = enif_make_list_cell(data->env, enif_make_binary(data->env, &bin), data->list);
	return 0;
}

ERL_NIF_TERM
geef_reference_glob(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	int error;
	geef_repository *repo;
	ErlNifBinary bin;
	struct list_data data;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return enif_make_badarg(env);

	if (!geef_terminate_binary(&bin))
		return geef_oom(env);

	data.env = env;
	data.list = enif_make_list(env, 0);

	error = git_reference_foreach_glob(repo->repo, (char *) bin.data, append_to_list, &data);

	enif_release_binary(&bin);
	if (error < 0)
		return geef_error(env);

	return data.list;
}

ERL_NIF_TERM
geef_reference_to_id(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_repository *repo;
	ErlNifBinary bin;
	git_oid id;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
		return enif_make_badarg(env);

	if (!geef_terminate_binary(&bin))
		return geef_oom(env);

	if (git_reference_name_to_id(&id, repo->repo, (char *)bin.data) < 0)
		return geef_error(env);

	if (geef_oid_bin(&bin, &id) < 0)
		return geef_oom(env);

	return enif_make_tuple2(env, atoms.ok, enif_make_binary(env, &bin));
}

ERL_NIF_TERM
geef_reference_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	geef_repository *repo;
	geef_ref *ref;
	ErlNifBinary name, target;
	ERL_NIF_TERM term_ref;
	int error, force;
	const char *pname, *ptarget;

	if (!enif_get_resource(env, argv[0], geef_repository_type, (void **) &repo))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[1], &name))
		return enif_make_badarg(env);

	if (!enif_inspect_iolist_as_binary(env, argv[3], &target))
		return enif_make_badarg(env);

	if (!geef_terminate_binary(&name))
		return geef_oom(env);

	ref = enif_alloc_resource(geef_ref_type, sizeof(geef_ref));

	force = enif_is_identical(argv[4], atoms.true);

	pname = (const char *) name.data;
	if (enif_is_identical(argv[2], atoms.oid)) {
		const git_oid *oid = (const git_oid *) target.data;
		error = git_reference_create(&ref->ref, repo->repo, pname, oid, force);
	} else if (enif_is_identical(argv[2], atoms.symbolic)) {
		if (!geef_terminate_binary(&target))
			return geef_oom(env);

		ptarget = (const char *) target.data;
		error = git_reference_symbolic_create(&ref->ref, repo->repo, pname, ptarget, force);		
		enif_release_binary(&target);
	} else {
		enif_release_binary(&target);
		enif_release_binary(&name);
		return enif_make_badarg(env);
	}

	enif_release_binary(&name);

	if (error < 0)
		return geef_error(env);

	return create_ref(env, ref);
}
