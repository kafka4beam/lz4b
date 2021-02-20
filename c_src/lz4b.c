#include "lz4b.h"
#include <strings.h>
#include <assert.h>

static ERL_NIF_TERM frame_compress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin, output;
  size_t cap = 0;
  LZ4F_errorCode_t ret;
  LZ4F_preferences_t * pref = NULL;
  ERL_NIF_TERM result;
  int opt;
  if ( !enif_inspect_binary(env, argv[0], &bin) )
    return ERROR_ATOM("inspect_input_fail");

  if (enif_is_tuple(env, argv[1])) {
      pref = malloc(sizeof(LZ4F_preferences_t));
      if ( ! pref ) {
        return ERROR_ATOM("enomem");
      }
      if (parse_epreference(env, pref, &argv[1]) != 1 )
        return ERROR_ATOM("bad_preference");

  } else if(enif_is_number(env, argv[1]) && enif_get_int(env, argv[1], &opt)
            && opt == 0) {
    pref = NULL;

  } else {
    return ERROR_ATOM("bad_preference");
  }

  if (bin.size < 4096)
    {
      cap = 4096;
    } else {
    cap = LZ4F_compressFrameBound((size_t) bin.size, pref);
  }

  if (!enif_alloc_binary(cap, &output)) {
    free(pref);
    return ERROR_ATOM("enomem");
  }

  ret = LZ4F_compressFrame(output.data, output.size,
                           bin.data, bin.size,
                           pref);

  free(pref);

  if (LZ4F_isError(ret))
    {
      enif_release_binary(&output);
      return ERROR_ATOM(LZ4F_getErrorName(ret));
    }

  result = enif_make_sub_binary(env, enif_make_binary(env,  &output), 0, ret);
  return SUCCESS(result);
}


static ERL_NIF_TERM frame_decompress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary bin, dest;
  ERL_NIF_TERM result;

  LZ4F_dctx * dctx;
  LZ4F_decompressOptions_t * dccopt = NULL;
  size_t size_dest = 0, size_src = LZ4F_HEADER_SIZE_MAX, step_size = 0, size_expand = 1024;
  size_t cnt_consumed = 0, cnt_produced = 0;
  if ( ! enif_inspect_binary(env, argv[0], &bin) )
    return ERROR_ATOM("inspect_binary_fail");

  if ( enif_is_number(env, argv[1])) {
    if( ! enif_get_uint(env, argv[1], (unsigned int *) &step_size))
      return ERROR_ATOM("bad_yield_size");
  } else if(enif_is_tuple(env, argv[1])) {
    //todo error handling
    dccopt = malloc(sizeof(LZ4F_decompressOptions_t));
    parse_edecompressOption(env, &step_size, &size_expand, dccopt, &argv[1]);
  } else {
    return ERROR_ATOM("badopts");
  }

  int apiret = LZ4F_createDecompressionContext(&dctx, LZ4F_VERSION);

  if (LZ4F_isError(apiret))
    {
      free(dccopt);
      return ERROR_ATOM(LZ4F_getErrorName(apiret));
    }

  if ( 0 == step_size ) {
    step_size =  bin.size;
  }

  // try get original size from frame info
  LZ4F_frameInfo_t * fi;
  fi = malloc(sizeof(LZ4F_frameInfo_t));
  if ( ! fi ) {
    return ERROR_ATOM("nomem");
  }

  apiret = LZ4F_getFrameInfo(dctx, fi, bin.data, &size_src);

  if (LZ4F_isError(apiret)) {
    free(dccopt);
    return ERROR_ATOM(LZ4F_getErrorName(apiret));
  }

  cnt_consumed += size_src;

  if (fi -> contentSize) {
    size_dest = fi-> contentSize;
  } else {
    size_dest = size_expand;
  }

  if (!enif_alloc_binary(size_dest, &dest)) {
    LZ4F_freeDecompressionContext(dctx);
    free(dccopt);
    free(fi);
    return ERROR_ATOM("nomem");
  }

  // init counter before decompression loop
  size_dest = dest.size;
  size_src = step_size;

  do {
    apiret = LZ4F_decompress(dctx, dest.data + cnt_produced, &size_dest,
                             bin.data + cnt_consumed, &size_src,
                             dccopt);

    if (LZ4F_isError(apiret))
      {
        LZ4F_freeDecompressionContext(dctx);
        enif_release_binary(&dest);
        free(dccopt);
        free(fi);
        return ERROR_ATOM(LZ4F_getErrorName(apiret));
      }

    cnt_consumed = cnt_consumed + size_src;
    cnt_produced = cnt_produced + size_dest;

    //printf("cnt_consumed: %lu, cnt_produced: %lu\n", cnt_consumed, cnt_produced);

    if(apiret == 0) {
      break;
    } else {
      if (step_size < apiret) {
        size_src = apiret;
      } else {
        size_src = step_size;
      }

      if ( (dest.size - cnt_produced) < size_expand/3 ) {
        enif_realloc_binary(&dest, cnt_produced + size_expand);
      }
      size_dest = dest.size - cnt_produced;
    }
  } while (cnt_consumed < bin.size);

  if( cnt_produced == dest.size) {
    result = enif_make_binary(env, &dest);
  } else {
    result = enif_make_sub_binary(env, enif_make_binary(env, &dest), 0, cnt_produced);
  }

  if ( 0 != LZ4F_freeDecompressionContext(dctx))
    {
      enif_release_binary(&dest);
      free(dccopt);
      free(fi);
      return ERROR_ATOM("badframe");
    }

  free(dccopt);
  free(fi);
  return SUCCESS(result);
}


static ERL_NIF_TERM frame_decompress_iter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  size_t size_done = -1, size_consumed, suggested;

  ErlNifBinary bin, dest;
  ERL_NIF_TERM ctx_r, tmp, result;

  LZ4F_decompressOptions_t * dccopt = NULL;
  LZ4F_errorCode_t ret;

  erlctx *ectx = NULL;

  if ( !enif_inspect_binary(env, argv[1], &bin) )
    return ERROR_ATOM("bad_binary");

  // init counter, provider to decompressor
  size_consumed = bin.size;
  size_done = bin.size * 2;

  if (!enif_alloc_binary(size_done, &dest)) {
    return ERROR_ATOM("nomem");
  }

  // start frame
  if (enif_is_atom(env, argv[0])) {
    ectx = enif_alloc_resource(ctx_resource_t, sizeof(erlctx));
    ret = LZ4F_createDecompressionContext(&ectx->ctx, LZ4F_VERSION);
    if (LZ4F_isError(ret))
      {
        return ERROR_ATOM(LZ4F_getErrorName(ret));
      }
  } else {
    // frame cont.
    if (!enif_get_resource(env, argv[0], ctx_resource_t, (void **)&ectx)) {
      return ERROR_ATOM("badctx");
    }

    if(!ectx -> ctx) {
      return ERROR_ATOM("null_dctx");
    }
  }

  suggested = LZ4F_decompress(ectx->ctx, dest.data, &size_done,
                              bin.data, &size_consumed,
                              dccopt
                              );

  // done with frame, free mem
  if (0 == suggested) {
    enif_release_resource(ectx);
    LZ4F_freeDecompressionContext(ectx->ctx);
    ctx_r = enif_make_atom(env, "done");
  } else {
    ctx_r = enif_make_resource(env, ectx);
  }

  tmp = enif_make_binary(env, &dest);
  result = enif_make_sub_binary(env, tmp, 0, size_done);

  return enif_make_tuple5(env,
                          ctx_r,
                          result,
                          enif_make_int(env, size_done),
                          enif_make_int(env, size_consumed),
                          enif_make_int(env, suggested));
}

static ERL_NIF_TERM eframeinfo(ErlNifEnv* env, const LZ4F_frameInfo_t* frameinfo) {
  return enif_make_tuple8(env,
                          enif_make_atom(env, "frame_info"),
                          enif_make_uint(env, frameinfo->blockSizeID),
                          enif_make_uint(env, frameinfo->blockMode),
                          enif_make_uint(env, frameinfo->contentChecksumFlag),
                          enif_make_uint(env, frameinfo->frameType),
                          enif_make_uint64(env, frameinfo->contentSize),
                          enif_make_uint(env, frameinfo->dictID),
                          enif_make_uint(env, frameinfo->blockChecksumFlag)
                          );
}

// note, we should never alloc mem in parser.
int parse_eframeinfo(ErlNifEnv* env, LZ4F_frameInfo_t *frameinfop, const ERL_NIF_TERM * eframeinfo) {
  const ERL_NIF_TERM * opts ; // see #frame_info in src/lz4b_frame.hrl
  int arity = 0;

  //parse opts)
  if (! enif_get_tuple(env, *eframeinfo, &arity, &opts)) {
    return -1; //not a tuple
  }

  if (0 == arity) {
    return -2;
  }

  enif_get_uint(env, *(opts+1), &(frameinfop -> blockSizeID));
  enif_get_uint(env, *(opts+2), &(frameinfop -> blockMode));
  enif_get_uint(env, *(opts+3), &(frameinfop -> contentChecksumFlag));
  enif_get_uint(env, *(opts+4), &(frameinfop -> frameType));
  ErlNifUInt64 contentSize = 0 ;
  enif_get_uint64(env, *(opts+5), &contentSize);
  frameinfop -> contentSize = contentSize ;
  enif_get_uint(env, *(opts+6), &(frameinfop -> dictID));
  enif_get_uint(env, *(opts+7), &(frameinfop -> blockChecksumFlag));
  return 1;
}

static ERL_NIF_TERM frame_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM tuple;
    LZ4F_frameInfo_t* fi;

    fi = malloc(sizeof(LZ4F_frameInfo_t));

    if (!fi) {
        return ERROR_ATOM("nomem");
    }

    if (!parse_eframeinfo(env, fi, &argv[0])) {
      free(fi);
      return ERROR_ATOM("nomem");
    }

    tuple = eframeinfo(env, fi);
    return tuple;
}


static ERL_NIF_TERM read_frame_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary bin;
  LZ4F_dctx * dctx;
  LZ4F_frameInfo_t * fi;

  fi = malloc(sizeof(LZ4F_frameInfo_t));
  if ( ! fi)
    {
      return ERROR_ATOM("nomem");
    }

  if ( ! enif_inspect_binary(env, argv[0], &bin) ) {
    free(fi);
    return ERROR_ATOM("inspect_binary_fail");
  }

  int apiret = LZ4F_createDecompressionContext(&dctx, LZ4F_VERSION);

  if (LZ4F_isError(apiret))
    {
      free(fi);
      return ERROR_ATOM(LZ4F_getErrorName(apiret));
    }

  size_t head_size = LZ4F_HEADER_SIZE_MAX;

  //todo check error
  LZ4F_getFrameInfo(dctx, fi, bin.data, &head_size);

  ERL_NIF_TERM tuple = eframeinfo(env, fi);
  free(fi);
  return tuple;
}

int parse_edecompressOption(ErlNifEnv* env,
                            size_t * stepsize,
                            size_t * buffgrow_size,
                            LZ4F_decompressOptions_t * dccopt,
                            const ERL_NIF_TERM * eterm) {
  const ERL_NIF_TERM * opts;
  int arity = 0;

  //parse opts
  if (! enif_get_tuple(env, *eterm, &arity, &opts)) {
    return -1;
  }

  // todo hardcode arity
  if (0 == arity) {
    return -2;
  }
  enif_get_uint(env, *(opts+1), (unsigned int *) stepsize);
  enif_get_uint(env, *(opts+2), (unsigned int *) buffgrow_size);
  enif_get_uint(env, *(opts+3), &(dccopt -> stableDst));
  enif_get_uint(env, *(opts+4), &(dccopt -> reserved[0]));
  enif_get_uint(env, *(opts+5), &(dccopt -> reserved[1]));
  enif_get_uint(env, *(opts+6), &(dccopt -> reserved[2]));
  return 1;
}

// note: we should never alloc memeory in parser!
int parse_epreference(ErlNifEnv* env, LZ4F_preferences_t * preferences,
                      const ERL_NIF_TERM * epreference) {
  const ERL_NIF_TERM * opts;
  int arity = 0;

  //parse opts)
  if (! enif_get_tuple(env, *epreference, &arity, &opts)) {
    return -1;
  }

  // todo hardcode arity
  if (0 == arity) {
    return -2;
  }

  if (!parse_eframeinfo(env, &(preferences->frameInfo), opts+1))
    {
      return -3;
    }
  enif_get_int( env, *(opts+2), &(preferences -> compressionLevel));
  enif_get_uint(env, *(opts+3), &(preferences -> autoFlush));
  enif_get_uint(env, *(opts+4), &(preferences -> reserved[0]));
  enif_get_uint(env, *(opts+5), &(preferences -> reserved[1]));
  enif_get_uint(env, *(opts+6), &(preferences -> reserved[2]));
  return 1;
}

static ErlNifFunc nif_funcs[] =
  {
    {"compress_frame",   2, frame_compress},
    {"decompress_frame", 2, frame_decompress},
    {"dirty_compress_frame", 2, frame_compress, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"dirty_decompress_frame", 2, frame_decompress, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"decompress_frame_iter", 2, frame_decompress_iter},
    {"frame_info", 1, frame_info},
    {"read_frame_info", 1, read_frame_info}
  };


static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM loadinfo)
{
  int ret_val = 0;
  ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  ctx_resource_t = enif_open_resource_type(env, NULL, "context_resource",
                                           NULL, // cleaner
                                           flags, NULL);
  return ret_val;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data)
{
}


ERL_NIF_INIT(lz4b_nif, nif_funcs, & on_load, NULL, &on_upgrade, &on_unload);
