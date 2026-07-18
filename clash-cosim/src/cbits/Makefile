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

# Some `iverilog` packages (e.g. Debian/Ubuntu's Icarus Verilog 12.0) ship an
# `iverilog-vpi` whose compiled-in CFLAGS contain a corrupt `0-3` token that
# breaks the compile. Detect that token and, only then, route through a wrapper
# that strips it; healthy toolchains call `$(VPI)` directly.
cosim_vpi.vpi: cosim_vpi.c
	@if $(VPI) --cflags 2>/dev/null | grep -q ' 0-3 '; then \
	  echo "warning: working around corrupt '0-3' flag in $(VPI) (see iverilog-vpi-sanitize)"; \
	  ./iverilog-vpi-sanitize $(VPI) --name=cosim_vpi cosim_vpi.c; \
	else \
	  $(VPI) --name=cosim_vpi cosim_vpi.c; \
	fi

libcosim_clash.so: cosim_clash.c
	$(CC) $(CFLAGS) -fPIC -o cosim_clash.o -c cosim_clash.c
	$(CC) $(CFLAGS) -shared -o libcosim_clash.so cosim_clash.o

# remove created files
clean:
	rm -rf *.vvp *.vpi *.o *.so *.sl *.hi
