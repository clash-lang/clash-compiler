#/bin/bash
cd $1
ghdl -i --workdir=work *.vhdl
