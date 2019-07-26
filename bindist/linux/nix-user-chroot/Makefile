ENV_PATH ?= "/bin"

nix-user-chroot: main.cpp
	g++ -o nix-user-chroot -DENV_PATH='$(ENV_PATH)' main.cpp
