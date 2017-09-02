if [ -f $HOME/iverilog/bin/vvp ]; then
  echo 'Using cached iverilog.';
else
  git clone -b v10-branch https://github.com/steveicarus/iverilog.git iverilog-source;
  cd iverilog-source && sh autoconf.sh && ./configure --prefix=$HOME/iverilog && make && make install && cd ..;
fi;
