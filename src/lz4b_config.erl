-module(lz4b_config).

-export([reload_config/0]).

-spec reload_config() -> ok.
-ifdef(OTP_RELEASE).
reload_config() ->
    Val = application:get_env(lz4b, dirty_threshold,0),
    true = is_integer(Val),
    ok = persistent_term:put({lz4b, dirty_threshold}, Val).
-else.
reload_config() -> ok.
-endif.
