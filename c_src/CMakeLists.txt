cmake_minimum_required(VERSION 3.17)
project(c_src)
set(CMAKE_CXX_STANDARD 14)
set(LZ4_DIR lz4-1.9.2)

include_directories(.)
find_library(lz4 1.9.3)
SET(Erlang_EI_INCLUDE_DIRS   ${Erlang_OTP_LIB_DIR}/${Erlang_EI_DIR}/include)
EXECUTE_PROCESS(
        COMMAND         erl -noshell -eval "io:format(\"~s\", [code:root_dir()])" -s erlang halt
        OUTPUT_VARIABLE Erlang_OTP_ROOT_DIR
)
include_directories(${Erlang_OTP_ROOT_DIR}/usr/include/ ${LZ4_DIR}/lib/)
add_library(lz4b_nif
        lz4b.c
        lz4b.h)
