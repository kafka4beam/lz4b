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

Copyright (C) 2018  Zhuwei Yang

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
