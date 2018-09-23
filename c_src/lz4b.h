#include <erl_nif.h>
#include <stdio.h>
#include <lz4.h>
#include <lz4frame.h>

struct ErlCtx {
  int count;
  LZ4F_dctx *ctx;
};

typedef struct ErlCtx erlctx;

static ErlNifResourceType *ctx_resource_t;

#define ERROR_ATOM(Err) enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_atom(env, Err));

#define SUCCESS(Term) enif_make_tuple(env, 2, enif_make_atom(env, "ok"), Term);


int parse_epreference(ErlNifEnv* env, LZ4F_preferences_t * preferences,
                      const ERL_NIF_TERM * epreference);

int parse_edecompressOption(ErlNifEnv* env,
                            size_t * buffgrow_size,
                            size_t * stepsize,
                            LZ4F_decompressOptions_t * dccopt,
                            const ERL_NIF_TERM * eterm);

int parse_eframeinfo(ErlNifEnv* env, LZ4F_frameInfo_t *frameinfop, const ERL_NIF_TERM * eframeinfo);
