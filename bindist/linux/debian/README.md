docker build . -t clashdeb 
docker run -v ~/code/clash-compiler/:/clash-compiler clashdeb /clash-compiler/bindist/linux/debian/mkBinDist.sh
