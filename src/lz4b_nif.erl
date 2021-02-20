-module(lz4b_nif).

-include("lz4b_frame.hrl").

%%% NIFs
-export([compress_frame/2,
         decompress_frame/2,
         decompress_frame_iter/2,
         dirty_compress_frame/2,
         dirty_decompress_frame/2,
         frame_info/1,
         read_frame_info/1
        ]).

-on_load(init/0).

-export_type([error_ret/0]).

-type error_ret() :: {error, ErrorIfo :: atom()}.

-type default_opt() :: 0.

-type decompress_opts() :: YieldSize::integer() | #decompress_options{}.

-type compress_opts() :: default_opt() | #compress_options{}.

init() ->
    Niflib = filename:join(filename:dirname(code:which(?MODULE)), "lz4b_nif"),
    ok = erlang:load_nif(Niflib, 0).

-spec compress_frame(binary(), compress_opts()) -> {ok, binary()} | error_ret().
compress_frame(_Binary, _Opts)  ->
    erlang:nif_error(nif_library_not_loaded).

-spec dirty_compress_frame(binary(), compress_opts()) -> {ok, binary()} | error_ret().
dirty_compress_frame(_Binary, _Opts)  ->
    erlang:nif_error(nif_library_not_loaded).

-spec decompress_frame(binary(), decompress_opts()) ->
                              {ok, binary()} | error_ret().
decompress_frame(_Binary, _Opts) ->
    erlang:nif_error(nif_library_not_loaded).

-spec dirty_decompress_frame(binary(), decompress_opts()) ->
                              {ok, binary()} | error_ret().
dirty_decompress_frame(_Binary, _Opts) ->
    erlang:nif_error(nif_library_not_loaded).

decompress_frame_iter(_Ref, _Bin) ->
    erlang:nif_error(nif_library_not_loaded).

frame_info(FI) when is_record(FI, frame_info)->
    erlang:nif_error(nif_library_not_loaded).

-spec read_frame_info(binary()) -> #frame_info{} | error_ret().
read_frame_info(_Bin) ->
    erlang:nif_error(nif_library_not_loaded).

%%% EUNIT

-ifdef(TEST).
-define(Fsize, 100).
-include_lib("eunit/include/eunit.hrl").

decompress_frame_test() ->
     {ok, Bin} = file:read_file("test_data/helloworld.lz4"),
     ?assertEqual({ok, << "helloworld\n" >>}, decompress_frame(Bin, 0)).

compress_test() ->
    Data = << "helloworld\n" >>,
    {ok, Bin} = file:read_file("test_data/helloworld.lz4"),
    FrameInfo = read_frame_info(Bin),
    ?assertEqual(#frame_info{
                    blocksize = 4,
                    blockMode = 1,
                    contentChksumFlag = 0
                   }, FrameInfo),
    {ok, Compressed} = compress_frame(Data, #compress_options{frameinfo = FrameInfo}),
    FrameInfo2 = read_frame_info(Compressed),
    ?assertEqual(#frame_info{
                    blocksize = 4,
                    blockMode = 1,
                    contentChksumFlag = 0
                   }, FrameInfo2),
    ?debugFmt("Expected:~n ~p ~nGet:~n ~p~n", [Bin, Compressed]),
    ?assertEqual(Compressed, Bin).

compress_with_check_sum_test() ->
    Data = << "helloworld\n" >>,
    {ok, Bin} = file:read_file("test_data/helloworld_cksum.lz4"),
    FrameInfo = read_frame_info(Bin),
    ?assertEqual(#frame_info{
                    blocksize = 4,
                    blockMode = 1,
                    contentChksumFlag = 1
                   }, FrameInfo),
    {ok, Compressed} = compress_frame(Data, #compress_options{frameinfo = FrameInfo}),
    FrameInfo2 = read_frame_info(Compressed),
    ?assertEqual(#frame_info{
                    blocksize = 4,
                    blockMode = 1,
                    contentChksumFlag = 1
                   }, FrameInfo2),
    ?debugFmt("Expected:~n ~p ~nGet:~n ~p~n", [Bin, Compressed]),

    ?assertEqual(Compressed, Bin).

compress_and_decompress_test() ->
    compress_and_decompress_test_helper(fun compress_frame/2, fun decompress_frame/2).

dirty_compress_and_decompress_test() ->
    compress_and_decompress_test_helper(fun dirty_compress_frame/2, fun dirty_decompress_frame/2).

compress_and_decompress_test_helper(CompressFun, DecompressFun) ->
    Data = list_to_binary(lists:flatten(lists:duplicate(1000,"abcdefg"))),
    {ok, Compressed} = CompressFun(Data, 0),
    %%?debugFmt("!!!before: ~p  after compressed : ~p~n",[byte_size(Data), byte_size(Compressed)]),
    timer:sleep(1000),
    Res = DecompressFun(Compressed, 0),
    %%?debugFmt("Expected:~n ~p ~nGet:~n ~p~n", [Data, Res]),
    ?assertEqual({ok, Data}, Res).

compress_and_decompress_with_opts_test() ->
    Data = list_to_binary(lists:flatten(lists:duplicate(1000,"abcdefg"))),
    {ok, Compressed} = compress_frame(Data, #compress_options{frameinfo =
                                                             #frame_info{contentsize = byte_size(Data)
                                                                        }}),
    ?assertEqual({ok,Data}, decompress_frame(Compressed, #decompress_options{})).

decompress_largefile_test()->
    {ok, Compressed} = file:read_file("test_data/large.lz4"),
    {ok, Decompressed} = file:read_file("test_data/large"),
    ?assertEqual({ok, Decompressed}, decompress_frame(Compressed, 0)).

decompress_with_buffergrow_size_test() ->
    Data = list_to_binary(lists:flatten(lists:duplicate(1000,"abcdefg"))),
    {ok, Compressed} = compress_frame(Data, 0),
    ?debugFmt("Test with buffergrow",[]),
    ?assertEqual({ok,Data}, decompress_frame(Compressed,
                                             #decompress_options{
                                                yield_size = 10,
                                                buffgrow_size = 512})).

decompress_with_opt_test() ->
    {ok, Compressed} = file:read_file("test_data/large.lz4"),
    {ok, Decompressed} = file:read_file("test_data/large"),
    ?assertEqual({ok, Decompressed},
                 decompress_frame(Compressed,
                                  #decompress_options{
                                     stableDst = 1,
                                     yield_size = 4096})).

decompress_file_with_content_size_test()->
    {ok, Bin} = file:read_file("test_data/bible.txt.lz4"),
    {ok, Expected} = file:read_file("test_data/bible.txt"),
    FrameInfo = read_frame_info(Bin),
    Contentsize = FrameInfo#frame_info.contentsize,
    ?assertNot(Contentsize == 0),
    {ok, Result} = decompress_frame(Bin, 0),
    ?assertEqual(Expected, Result).

compress_perf_test() ->
    Work = erlang:system_info(dirty_cpu_schedulers_online)
        + erlang:system_info(schedulers_online),
    Count = Work * 10,
    {ok, Plain} = file:read_file("test_data/bible.txt"),
    Start = os:timestamp(),
    [compress_frame(Plain, 0) || _ <- lists:seq(1, Count)],
    End = os:timestamp(),
    ?debugFmt("each compress takes ~p us", [timer:now_diff(End, Start) / Count]).

compress_perf_parallel_test() ->
    Work = erlang:system_info(dirty_cpu_schedulers_online)
        + erlang:system_info(schedulers_online),
    Count = Work * 10,
    {ok, Plain} = file:read_file("test_data/bible.txt"),
    Start = os:timestamp(),
    Owner = self(),
    Workers = [spawn(fun() -> compress_frame(Plain, 0), Owner ! {self(), done} end)
               || _ <- lists:seq(1, Count)],
    wait_for(Workers),
    End = os:timestamp(),
    ?debugFmt("each parallel compress takes ~p us", [timer:now_diff(End, Start) / Count]).

dirty_compress_perf_test() ->
    Count = 10,
    {ok, Plain} = file:read_file("test_data/bible.txt"),
    Start = os:timestamp(),
    [compress_frame(Plain, 0) || _ <- lists:seq(1, Count)],
    End = os:timestamp(),
    ?debugFmt("each dirty compress takes ~p us", [timer:now_diff(End, Start) / Count]).

dirty_compress_perf_parallel_test() ->
    Work = erlang:system_info(dirty_cpu_schedulers_online)
        + erlang:system_info(schedulers_online),
    Count = Work * 10,
    {ok, Plain} = file:read_file("test_data/bible.txt"),
    Start = os:timestamp(),
    Owner = self(),
    Workers = [spawn(fun() -> dirty_compress_frame(Plain, 0), Owner ! {self(), done} end)
               || _ <- lists:seq(1, Count)],
    wait_for(Workers),
    End = os:timestamp(),
    ?debugFmt("each parallel dirty compress takes ~p us", [timer:now_diff(End, Start) / Count]).

-ifdef(OTP_RELEASE).
dirty_threshold_compress_perf_parallel_test() ->
    application:set_env(lz4b, dirty_threshold, 1024),
    lz4b_config:reload_config(),
    Work = erlang:system_info(dirty_cpu_schedulers_online)
        + erlang:system_info(schedulers_online),
    Count = Work * 10,
    {ok, Plain} = file:read_file("test_data/bible.txt"),
    Start = os:timestamp(),
    Owner = self(),
    Workers = [spawn(fun() -> compress_frame(Plain, 0), Owner ! {self(), done} end)
               || _ <- lists:seq(1, Count)],
    wait_for(Workers),
    End = os:timestamp(),
    application:set_env(lz4b, dirty_threshold, 0),
    lz4b_config:reload_config(),
    ?debugFmt("each compress with dirty threshold takes ~p us", [timer:now_diff(End, Start) / Count]).
-endif.

decompress_with_bad_opts_test() ->
    {ok, Compressed} = file:read_file("test_data/large.lz4"),
    ?assertEqual({error, badopts},
                 decompress_frame(Compressed,
                                  badopts)),
    ?assertEqual({error, bad_yield_size},
                 decompress_frame(Compressed,
                                  -1)).

decompress_part_test() ->
    {ok, Compressed} = file:read_file("test_data/large.lz4"),
    {ok, Expected} = file:read_file("test_data/large"),
    ?debugVal(byte_size(Compressed)),
    R = decompress_part_helper(Compressed, 0, decompress_frame_iter(start, binary:part(Compressed, 0, ?Fsize)), << >>),
    ?assertEqual(Expected, R).

decompress_badframe_test_()->
    [?_assertEqual({error, 'ERROR_frameType_unknown'}, decompress_frame(<<0>>, 0)),
     ?_assertEqual({error, 'ERROR_frameType_unknown'}, decompress_frame(<<1,2,3,4>>, 0))
    ].

default_frame_info_test() ->
    ?assertEqual(#frame_info{} , frame_info(#frame_info{})).

frame_info_test() ->
    ?assertEqual(#frame_info{blockMode=1,contentChksumFlag = 1} ,
                 frame_info(#frame_info{blockMode=1,contentChksumFlag = 1})).

bad_preference_test() ->
    ?assertEqual({error, bad_preference} ,
                 compress_frame(<<"hello world">>, a)).

bad_preference_2_test() ->
    ?assertEqual({error, bad_preference} ,
                 compress_frame(<<"hello world">>, [])).

bad_compress_3_test() ->
    ?assertEqual({error, inspect_input_fail} ,
                 compress_frame("hello world", 0)).

read_frame_info_test() ->
    {ok, Bin} = file:read_file("test_data/helloworld.lz4"),
    ?assertEqual(#frame_info{
                    blocksize = 4,
                    blockMode = 1,
                    contentChksumFlag = 0
                   }, read_frame_info(Bin)).

nif_upgrade_test() ->
    ?assertEqual({module, ?MODULE},code:load_file(?MODULE)).


memory_leak_default_opts_test_() ->
    {ok, Compressed} = file:read_file("test_data/helloworld.lz4"),
    Data= << "helloworld\n" >>,
    {timeout, 60000,
     fun()->
             Fun = fun() ->
                           {ok, _} = compress_frame(Data, 0),
                           {ok, _} = decompress_frame(Compressed, 0)
                   end,
             do_mem_leak(1000000, Fun)
     end }.

memory_leak_test_() ->
    {ok, Compressed} = file:read_file("test_data/helloworld.lz4"),
    Data= << "helloworld\n" >>,
    {timeout, 60000,
     fun()->
             Fun = fun() ->
                           {ok, _} = compress_frame(Data, #compress_options{}),
                           {ok, _} = decompress_frame(Compressed, #decompress_options{})
                   end,
             do_mem_leak(1000000, Fun)
     end }.

do_mem_leak(0, _F) ->
    RssNow = get_rss_kb(),
    Rss100k = get(rss10),
    Rss100k < RssNow andalso (RssNow / Rss100k) > 1.2 andalso error(mem_leak),
    ok;
do_mem_leak(10 = Cnt, F) -> %% take sample
    put(rss10, get_rss_kb()),
    do_mem_leak(Cnt-1, F);
do_mem_leak(X, F) ->
    case os:type() of
        {unix, darwin} ->
            skip;
        {win32, darwin} ->
            skip;
        _ ->
            F(),
            do_mem_leak(X-1, F)
    end.

get_rss_kb() ->
    [$\t | Out] = os:cmd("cat /proc/self/status  |grep -i vmrss | cut -f2 -d:"),
    Out1 = string:strip(Out, both),
    Out2 = string:strip(Out1, both, $\n),
    [Num, Unit] = string:tokens(Out2, " "),
    case Unit of
        "kB" ->
            list_to_integer(Num);
        "mB" -> %% I am not sure, if mB wil present
            list_to_integer(Num)*1024
    end.

decompress_part_helper(_RawData, _Offset, {_Ref, Bin1, _CntDone, _CntConsumed, 0 = _Suggested}, Res) ->
    ?debugMsg("side by side decompress done"),
    <<Res/binary, Bin1/binary >>;
decompress_part_helper(RawData, Offset, {Ref, Bin1, CntDone, CntConsumed, Suggested}, Res) ->
    ?debugFmt("from ~p, to ~p, done: ~p, consumed: ~p next_sugg: ~p ~n, ", [Offset, Offset+CntConsumed, CntDone, CntConsumed, Suggested]),
    Loc = Offset + CntConsumed,
    NextBlock = case byte_size(RawData) of
                    S when Loc == S ->
                        ?debugMsg("side by side decompress done"),
                        <<Res/binary, Bin1/binary >>;
                    S when Loc+Suggested < S ->
                        ?debugFmt("using seg with suggest: ~p -> ~p~n", [Loc, Loc + Suggested]),
                        binary:part(RawData, Loc, Suggested);
                    S when Loc+?Fsize > S  ->
                        ?debugFmt("using seg: ~p -> ~p~n", [S, Loc-S]),
                        binary:part(RawData, S, Loc-S);
                    _ ->
                        ?debugFmt("using seg: ~p -> ~p~n", [Loc, Loc+?Fsize]),
                        binary:part(RawData, Loc, ?Fsize)
                end,
    decompress_part_helper(RawData, Loc, decompress_frame_iter(Ref, NextBlock), << Res/binary, Bin1/binary>>).

wait_for([]) ->
    ok;
wait_for(Workers) ->
    receive
        {W, done} ->
            wait_for(Workers -- [W])
    end.

-endif.
