LZ4DIR = lz4

include env.mk

all: lz4b_nif.so

clean:
	-rm *.so
	-rm *.o
	-$(MAKE) -C $(LZ4DIR) clean

lz4b_nif.so: lz4_lib lz4b.o
	@$(LD) -shared -o $@ lz4/lib/*.o lz4b.o

%.o: %.c %.h
	@$(CC) -Wall -fPIC -O0 -g -c -o $@ $< -I$(ERTS_INCLUDE_DIR) -Ilz4/lib/

lz4_lib: lz4/lib/
	@CFLAGS="-fPIC -save-temps"  $(MAKE) -C $(LZ4DIR) BUILD_STATIC=no
