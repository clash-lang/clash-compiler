# Rules
all:: clash

CC = gcc
VPI = iverilog-vpi
CLASH = clash --interactive
CFLAGS = -Wall -O2

cosim_vpi.sl: cosim_vpi.c
	$(CC) -o cosim_vpi.o -c -m32 -fPIC -fno-stack-protector -g cosim_vpi.c 
	ld -melf_i386 -shared -E -o cosim_vpi.sl cosim_vpi.o

cosim_vpi.vpi: cosim_vpi.c
	$(VPI) --name=cosim_vpi cosim_vpi.c
	
libcosim_clash.so: cosim_clash.c 
	$(CC) $(CFLAGS) -fPIC -o cosim_clash.o -c cosim_clash.c 
	$(CC) $(CFLAGS) -shared -o libcosim_clash.so cosim_clash.o

clash: cosim_vpi.vpi libcosim_clash.so CoSimCLaSH.hs CoSimTest.hs
	$(CLASH) CoSimTest.hs CoSimCLaSH.hs -lcosim_clash -L.

# remove created files
clean:
	rm -rf *.vvp *.vpi *.o *.so *.sl *.hi