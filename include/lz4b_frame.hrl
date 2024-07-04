-type frame_blockSize() :: integer().
-type frame_blockMode() :: integer().
-type frame_contentChecksum() :: integer().
-type frame_type() :: integer().
-type frame_contentSize() :: integer().
-type frame_dictId() :: integer().
-type frame_blockChecksum()  :: integer().


-record(frame_info, {blocksize = 0 :: frame_blockSize(),
                     blockMode = 0 :: frame_blockMode(),
                     contentChksumFlag = 0 :: frame_contentChecksum(),
                     frametype = 0 :: frame_type(),
                     contentsize = 0 :: frame_contentSize(),
                     dictID = 0 :: frame_dictId(),
                     blockCheckSum = 0 :: frame_blockChecksum()
                    }).

-record(compress_options, {frameinfo = #frame_info{},
                      compression_level = 0 :: integer(),
                      autoflush = 0,
                      reserved1 = 0,
                      reserved2 = 0,
                      reserved3 = 0
                     }).

-record(decompress_options, {yield_size = 0 :: pos_integer(),
                             buffgrow_size = 2048 :: pos_integer(),
                             stableDst = 0 :: pos_integer(),
                             reserved1 = 0 :: pos_integer(),
                             reserved2 = 0 :: pos_integer(),
                             reserved3 = 0 :: pos_integer()
                            }).
