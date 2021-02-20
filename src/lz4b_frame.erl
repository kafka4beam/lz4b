-module(lz4b_frame).
-include("lz4b_frame.hrl").
-export([compress/1,
         decompress/1,
         decompress/2,
         incremental_decompress/3,
         read_frame_info/1
        ]).

-on_load(init/0).

-type error_ret() :: lz4b_nif:error_ret().

init() ->
    lz4b_config:reload_config().

-spec decompress(iodata()) -> {ok, binary()} | lz4b_nif:error_ret().
decompress(IoData) ->
    decompress(IoData, 0).

-spec decompress(iodata(), Options :: integer() | #decompress_options{})
                -> {ok, binary()} | error_ret().
decompress(IoData, Opts) when not is_binary(IoData) ->
    decompress(iolist_to_binary(IoData), Opts);
decompress(Bin, Opts) ->
    case above_threshold(byte_size(Bin)) of
        true ->
            lz4b_nif:dirty_decompress_frame(Bin, Opts);
        _ ->
            lz4b_nif:decompress_frame(Bin, Opts)
    end.

-spec compress(iodata()) -> {ok, binary()} | error_ret().
compress(IoData) ->
    compress(IoData, 0).

-spec compress(iodata(), CompressOpts :: #compress_options{} | 0)
              -> {ok, binary()} | error_ret().
compress(IoData, Opts) when not is_binary(IoData) ->
    compress(iolist_to_binary(IoData), Opts);
compress(Bin, Opts) ->
    case above_threshold(byte_size(Bin)) of
        true ->
            lz4b_nif:dirty_compress_frame(Bin, Opts);
        _ ->
            lz4b_nif:compress_frame(Bin, Opts)
    end.

-spec read_frame_info(binary()) -> #frame_info{} | error_ret().
read_frame_info(Bin) ->
    lz4b_nif:read_frame_info(Bin).

-spec incremental_decompress(Reader :: fun(),
                             Writer :: fun(),
                             Readsize :: pos_integer()) ->
                                    done.
incremental_decompress(ReaderFun, WriterFun, Readsize)->
    do_incremental_decompress(ReaderFun, WriterFun, Readsize, start, << >>).

do_incremental_decompress(ReaderFun, WriterFun, Readsize, Ref, LeftBin)->
    case ReaderFun(Readsize) of
        eof when LeftBin == 0->
            done;
        Other ->
            InputBin = case Other of
                           {ok, X} ->
                               <<LeftBin/binary, X/binary>>;
                           eof ->
                               LeftBin
                       end,
            case lz4b_nif:decompress_frame_iter(Ref, InputBin) of
                {_NewRef, BinDecompressed, _CntDone, _CntConsumed, 0}  ->
                    WriterFun(BinDecompressed),
                    done;
                {NewRef, BinDecompressed, _CntDone, CntConsumed, _Suggested} when CntConsumed == byte_size(InputBin) ->
                    WriterFun(BinDecompressed),
                    do_incremental_decompress(ReaderFun, WriterFun, Readsize, NewRef, << >>);
                {NewRef, BinDecompressed, _CntDone, CntConsumed, _Suggested} when CntConsumed < byte_size(InputBin) ->
                    InputSize = byte_size(InputBin),
                    Left = binary:part(InputBin, InputSize, CntConsumed - InputSize),
                    WriterFun(BinDecompressed),
                    do_incremental_decompress(ReaderFun, WriterFun, Readsize, NewRef, Left)
            end
    end.

-spec above_threshold(integer()) -> boolean().

-ifdef(OTP_RELEASE).
above_threshold(Current) ->
    case persistent_term:get({lz4b, dirty_threshold}, 0) of
        0 -> %% disabled
            false;
        Threshold when Current > Threshold ->
            true;
        _ ->
            false
    end.
-else.
above_threshold(_) -> false.
-endif.

%%% EUNIT

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decompress_test() ->
     {ok, Bin} = file:read_file("test_data/helloworld.lz4"),
     ?assertEqual({ok, << "helloworld\n" >>}, decompress(Bin)).

compress_and_decompress_test() ->
    Data = <<"abcdefg">>,
    {ok, Compressed} = compress(Data),
    ?assertEqual({ok,Data}, decompress(Compressed)).

decompress_largefile_test()->
    {ok, Compressed} = file:read_file("test_data/large.lz4"),
    {ok, Decompressed} = file:read_file("test_data/large"),
    ?assertEqual({ok, Decompressed}, decompress(Compressed)).

incremental_decompress_test() ->
    TestFile = "test_data/large.lz4",
    Outfile = "test_data/large.out",
    ExpectedFile = "test_data/large",
    SegSize = 200,
    {ok, Fd} = file:open(TestFile, [read, raw, binary]),
    {ok, Fdout} = file:open(Outfile, [write, raw, binary]),
    Reader = fun(Size) ->
                     file:read(Fd, Size)
             end,

    Writer = fun(Bin) ->
                     file:write(Fdout, Bin)
             end,
    done = incremental_decompress(Reader, Writer, SegSize),
    file:close(Fd),
    file:close(Fdout),
    {ok, Expected} = file:read_file(ExpectedFile),
    {ok, Res} = file:read_file(Outfile),
    ?assertEqual(Expected, Res).


incremental_decompress_big_test_() ->
    TestFile = "test_data/large_txt.lz4",
    Outfile = "test_data/large_txt.out",
    ExpectedFile = "test_data/large_txt",
    {timeout, 3000,
     [fun() ->
              SegSize = 4096,
              {ok, Fd} = file:open(TestFile, [read, raw, binary]),
              {ok, Fdout} = file:open(Outfile, [write, raw, binary]),
              Reader = fun(Size) ->
                               file:read(Fd, Size)
                       end,

              Writer = fun(Bin) ->
                               file:write(Fdout, Bin)
                       end,
              {Time, done} = timer:tc(fun()->
                                              incremental_decompress(Reader, Writer, SegSize)
                                      end),
              ?debugFmt("spent: ~p~n", [Time]),
              file:close(Fd),
              file:close(Fdout),
              {ok, Expected} = file:read_file(ExpectedFile),
              {ok, Res} = file:read_file(Outfile),
              ?assertEqual(Expected, Res)
      end]}.

frame_info_test() ->
    ?assertEqual(#frame_info{},
                 lz4b_nif:frame_info(#frame_info{})).

read_frame_info_test() ->
    {ok, Bin} = file:read_file("test_data/helloworld.lz4"),
    ?assertEqual( #frame_info{
                     blocksize = 4,
                     blockMode = 1,
                     contentChksumFlag = 0
                    }, read_frame_info(Bin)).

-endif.
