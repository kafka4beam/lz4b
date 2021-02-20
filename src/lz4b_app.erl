-module(lz4b_app).
-export([ start/2
        , reload_config/0
        , stop/0]).

start(_, _)->
    reload_config().
stop()->
    ok.

-ifdef(OTP_RELEASE).
reload_config() ->
    {ok, Val} = application:get_env(lz4b, dirty_threshold),
    true = is_integer(Val),
    ok = persistent_term:put({lz4b, dirty_threshold}, Val).
-else.
reload_config() -> ok.
-endif.
