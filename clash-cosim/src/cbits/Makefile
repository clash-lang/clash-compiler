# Rules
all:: cosim_vpi.vpi libcosim_clash.so cosim_vpi.sl

CC = gcc
VPI = iverilog-vpi
CFLAGS = -Wall -O2

UNAME = $(shell uname)
ifeq ($(UNAME),Darwin)
  LDFLAGS = -L /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib/ -lSystem -dylib -export_dynamic -flat_namespace -undefined suppress -macosx_version_min 10.14
else
  LDFLAGS = -shared -E
endif

cosim_vpi.sl: cosim_vpi.c
	$(CC) $(CFLAGS) -o cosim_vpi.o -c -fPIC -fno-stack-protector -g cosim_vpi.c
	ld $(LDFLAGS) -o cosim_vpi.sl cosim_vpi.o

cosim_vpi.vpi: cosim_vpi.c
	$(VPI) --name=cosim_vpi cosim_vpi.c

libcosim_clash.so: cosim_clash.c
	$(CC) $(CFLAGS) -fPIC -o cosim_clash.o -c cosim_clash.c
	$(CC) $(CFLAGS) -shared -o libcosim_clash.so cosim_clash.o

# remove created files
clean:
	rm -rf *.vvp *.vpi *.o *.so *.sl *.hi
