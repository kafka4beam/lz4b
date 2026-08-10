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
      pref = calloc(1, sizeof(LZ4F_preferences_t));
      if ( ! pref ) {
        return ERROR_ATOM("enomem");
      }
      if (parse_epreference(env, pref, &argv[1]) != 1 ) {
        free(pref);
        return ERROR_ATOM("bad_preference");
      }

  } else if(enif_is_number(env, argv[1]) && enif_get_int(env, argv[1], &opt)
            && opt == 0) {
    pref = NULL;

  } else {
    return ERROR_ATOM("bad_preference");
  }

  cap = LZ4F_compressFrameBound((size_t) bin.size, pref);

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

  LZ4F_dctx * dctx = NULL;
  LZ4F_decompressOptions_t * dccopt = NULL;
  LZ4F_frameInfo_t fi;
  size_t size_dest = 0, size_src = 0, step_size = 0, size_expand = 1024;
  size_t cnt_consumed = 0, cnt_produced = 0;
  size_t apiret;
  const char * error_name = NULL;
  int dest_allocated = 0;
  if ( ! enif_inspect_binary(env, argv[0], &bin) )
    return ERROR_ATOM("inspect_binary_fail");

  if ( enif_is_number(env, argv[1])) {
    if( ! enif_get_uint(env, argv[1], (unsigned int *) &step_size))
      return ERROR_ATOM("bad_yield_size");
  } else if(enif_is_tuple(env, argv[1])) {
    dccopt = calloc(1, sizeof(LZ4F_decompressOptions_t));
    if (!dccopt) {
      return ERROR_ATOM("nomem");
    }
    if (parse_edecompressOption(env, &step_size, &size_expand, dccopt, &argv[1]) != 1) {
      free(dccopt);
      return ERROR_ATOM("badopts");
    }
  } else {
    return ERROR_ATOM("badopts");
  }

  apiret = LZ4F_createDecompressionContext(&dctx, LZ4F_VERSION);

  if (LZ4F_isError(apiret))
    {
      free(dccopt);
      return ERROR_ATOM(LZ4F_getErrorName(apiret));
    }

  if ( 0 == step_size ) {
    step_size =  bin.size;
  }

  // try get original size from frame info
  size_src = bin.size < LZ4F_HEADER_SIZE_MAX ? bin.size : LZ4F_HEADER_SIZE_MAX;
  apiret = LZ4F_getFrameInfo(dctx, &fi, bin.data, &size_src);

  if (LZ4F_isError(apiret)) {
    error_name = LZ4F_getErrorName(apiret);
    LZ4F_freeDecompressionContext(dctx);
    free(dccopt);
    return ERROR_ATOM(error_name);
  }

  cnt_consumed += size_src;

  if (fi.contentSize) {
    size_dest = fi.contentSize;
  } else {
    size_dest = size_expand;
  }

  if (!enif_alloc_binary(size_dest, &dest)) {
    LZ4F_freeDecompressionContext(dctx);
    free(dccopt);
    return ERROR_ATOM("nomem");
  }
  dest_allocated = 1;

  do {
    size_t remaining = bin.size - cnt_consumed;
    size_src = step_size < remaining ? step_size : remaining;
    size_dest = dest.size - cnt_produced;

    apiret = LZ4F_decompress(dctx, dest.data + cnt_produced, &size_dest,
                             bin.data + cnt_consumed, &size_src,
                             dccopt);

    if (LZ4F_isError(apiret))
      {
        error_name = LZ4F_getErrorName(apiret);
        goto error;
      }

    cnt_consumed = cnt_consumed + size_src;
    cnt_produced = cnt_produced + size_dest;

    //printf("cnt_consumed: %lu, cnt_produced: %lu\n", cnt_consumed, cnt_produced);

    if(apiret != 0) {
      if (dest.size == cnt_produced ||
          (dest.size - cnt_produced) < size_expand/3) {
        if (cnt_produced > SIZE_MAX - size_expand ||
            !enif_realloc_binary(&dest, cnt_produced + size_expand)) {
          error_name = "nomem";
          goto error;
        }
      }
    }
  } while (apiret != 0 && cnt_consumed < bin.size);

  if (apiret != 0) {
    error_name = "incomplete_frame";
    goto error;
  }

  if( cnt_produced == dest.size) {
    result = enif_make_binary(env, &dest);
  } else {
    result = enif_make_sub_binary(env, enif_make_binary(env, &dest), 0, cnt_produced);
  }

  if ( 0 != LZ4F_freeDecompressionContext(dctx))
    {
      enif_release_binary(&dest);
      free(dccopt);
      return ERROR_ATOM("badframe");
    }

  free(dccopt);
  return SUCCESS(result);

error:
  LZ4F_freeDecompressionContext(dctx);
  if (dest_allocated) {
    enif_release_binary(&dest);
  }
  free(dccopt);
  return ERROR_ATOM(error_name);
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
  unsigned int block_size, block_mode, content_checksum, frame_type, dict_id, block_checksum;
  ErlNifUInt64 content_size;

  //parse opts)
  if (! enif_get_tuple(env, *eframeinfo, &arity, &opts)) {
    return -1; //not a tuple
  }

  if (arity != 8 ||
      !enif_is_identical(opts[0], enif_make_atom(env, "frame_info"))) {
    return -2;
  }

  if (!enif_get_uint(env, opts[1], &block_size) ||
      !enif_get_uint(env, opts[2], &block_mode) ||
      !enif_get_uint(env, opts[3], &content_checksum) ||
      !enif_get_uint(env, opts[4], &frame_type) ||
      !enif_get_uint64(env, opts[5], &content_size) ||
      !enif_get_uint(env, opts[6], &dict_id) ||
      !enif_get_uint(env, opts[7], &block_checksum)) {
    return -3;
  }

  frameinfop->blockSizeID = block_size;
  frameinfop->blockMode = block_mode;
  frameinfop->contentChecksumFlag = content_checksum;
  frameinfop->frameType = frame_type;
  frameinfop->contentSize = content_size;
  frameinfop->dictID = dict_id;
  frameinfop->blockChecksumFlag = block_checksum;
  return 1;
}

static ERL_NIF_TERM frame_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    LZ4F_frameInfo_t fi = {0};

    if (parse_eframeinfo(env, &fi, &argv[0]) != 1) {
      return ERROR_ATOM("badarg");
    }

    return eframeinfo(env, &fi);
}


static ERL_NIF_TERM read_frame_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary bin;
  LZ4F_dctx * dctx;
  LZ4F_frameInfo_t fi;
  size_t ret;

  if ( ! enif_inspect_binary(env, argv[0], &bin) ) {
    return ERROR_ATOM("inspect_binary_fail");
  }

  ret = LZ4F_createDecompressionContext(&dctx, LZ4F_VERSION);

  if (LZ4F_isError(ret))
    {
      return ERROR_ATOM(LZ4F_getErrorName(ret));
    }

  size_t head_size = bin.size < LZ4F_HEADER_SIZE_MAX ? bin.size : LZ4F_HEADER_SIZE_MAX;
  ret = LZ4F_getFrameInfo(dctx, &fi, bin.data, &head_size);

  if (LZ4F_isError(ret)) {
    const char * error_name = LZ4F_getErrorName(ret);
    LZ4F_freeDecompressionContext(dctx);
    return ERROR_ATOM(error_name);
  }

  ERL_NIF_TERM tuple = eframeinfo(env, &fi);
  LZ4F_freeDecompressionContext(dctx);
  return tuple;
}

int parse_edecompressOption(ErlNifEnv* env,
                            size_t * stepsize,
                            size_t * buffgrow_size,
                            LZ4F_decompressOptions_t * dccopt,
                            const ERL_NIF_TERM * eterm) {
  const ERL_NIF_TERM * opts;
  int arity = 0;
  unsigned int parsed_stepsize, parsed_buffgrow_size;
  unsigned int stable_dst, skip_checksums, reserved1, reserved0;

  //parse opts
  if (! enif_get_tuple(env, *eterm, &arity, &opts)) {
    return -1;
  }

  if (arity != 7 ||
      !enif_is_identical(opts[0], enif_make_atom(env, "decompress_options"))) {
    return -2;
  }

  if (!enif_get_uint(env, opts[1], &parsed_stepsize) ||
      !enif_get_uint(env, opts[2], &parsed_buffgrow_size) ||
      !enif_get_uint(env, opts[3], &stable_dst) ||
      !enif_get_uint(env, opts[4], &skip_checksums) ||
      !enif_get_uint(env, opts[5], &reserved1) ||
      !enif_get_uint(env, opts[6], &reserved0) ||
      parsed_buffgrow_size == 0) {
    return -3;
  }

  *stepsize = parsed_stepsize;
  *buffgrow_size = parsed_buffgrow_size;
  dccopt->stableDst = stable_dst;
  dccopt->skipChecksums = skip_checksums;
  dccopt->reserved1 = reserved1;
  dccopt->reserved0 = reserved0;
  return 1;
}

// note: we should never alloc memeory in parser!
int parse_epreference(ErlNifEnv* env, LZ4F_preferences_t * preferences,
                      const ERL_NIF_TERM * epreference) {
  const ERL_NIF_TERM * opts;
  int arity = 0;
  int compression_level;
  unsigned int auto_flush, reserved0, reserved1, reserved2;

  //parse opts)
  if (! enif_get_tuple(env, *epreference, &arity, &opts)) {
    return -1;
  }

  if (arity != 7 ||
      !enif_is_identical(opts[0], enif_make_atom(env, "compress_options"))) {
    return -2;
  }

  if (parse_eframeinfo(env, &preferences->frameInfo, &opts[1]) != 1 ||
      !enif_get_int(env, opts[2], &compression_level) ||
      !enif_get_uint(env, opts[3], &auto_flush) ||
      !enif_get_uint(env, opts[4], &reserved0) ||
      !enif_get_uint(env, opts[5], &reserved1) ||
      !enif_get_uint(env, opts[6], &reserved2)) {
    return -3;
  }

  preferences->compressionLevel = compression_level;
  preferences->autoFlush = auto_flush;
  preferences->reserved[0] = reserved0;
  preferences->reserved[1] = reserved1;
  preferences->reserved[2] = reserved2;
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
