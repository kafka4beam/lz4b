# lz4b: lz4 erlang binding

# Frame APIs

## Frame Compress

``` erlang
lz4b_frame:compress(binary()) -> {ok, binary()} | {error, atom()}.
```

## Frame Decompress
``` erlang
lz4b_frame:decompress(binary()) -> {ok, binary()} | {error, atom()}.
```

## Frame Info
``` erlang
lz4b_frame:read_frame_info(binary()) -> frame_info() | {error, atom()}.
```

# Configuration API

## reload configuration

``` erlang
lz4b_frame:reload_config() -> ok.
```

## Configuration
application env: 
`dirty_threshold` : In byte size. binary larger than this size would be handled in a dirty scheduler to avoid long scheduling in erlang scheduler. Default is 0 which means disabled (dirty scheduler not in use).





    
