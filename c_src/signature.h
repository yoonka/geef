#include "geef.h"

#ifndef GEEF_SIGNATURE_H
#define GEEF_SIGNATURE_H

ERL_NIF_TERM geef_signature_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM geef_signature_default(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
int geef_signature_from_erl(git_signature **out, ErlNifEnv *env, ERL_NIF_TERM *err, ERL_NIF_TERM term);

#endif
