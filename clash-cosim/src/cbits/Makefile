# Rules
all:: cosim_vpi.vpi libcosim_clash.so cosim_vpi.sl

CC = gcc
VPI = iverilog-vpi
CFLAGS = -Wall -O2

cosim_vpi.sl: cosim_vpi.c
	$(CC) -o cosim_vpi.o -c -m32 -fPIC -fno-stack-protector -g cosim_vpi.c 
	ld -melf_i386 -shared -E -o cosim_vpi.sl cosim_vpi.o

cosim_vpi.vpi: cosim_vpi.c
	$(VPI) --name=cosim_vpi cosim_vpi.c
	
libcosim_clash.so: cosim_clash.c 
	$(CC) $(CFLAGS) -fPIC -o cosim_clash.o -c cosim_clash.c 
	$(CC) $(CFLAGS) -shared -o libcosim_clash.so cosim_clash.o

# remove created files
clean:
	rm -rf *.vvp *.vpi *.o *.so *.sl *.hi
