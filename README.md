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

## Copyright and License

Copyright (C) 2018  Zhuwei Yang and contributors

Licensed under the Apache License, Version 2.0 ([LICENSE](LICENSE) or
<http://www.apache.org/licenses/LICENSE-2.0>).

The bundled [lz4](https://github.com/lz4/lz4) library sources are downloaded at
build time; only the `lib/` part of lz4 is compiled and linked into the NIF,
which is licensed under the BSD 2-Clause license (lz4's GPL-2.0-or-later
license applies only to its `programs/`, `tests/` and `examples/` directories,
none of which are used by this project).

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be licensed as above, without any additional terms or conditions.
