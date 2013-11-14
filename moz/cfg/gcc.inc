export CFLAGS="$CFLAGS -B$topsrcdir/../cfg/goldpath -fuse-ld=gold"
export CXXFLAGS="$CXXFLAGS -B$topsrcdir/../cfg/goldpath -fuse-ld=gold"
export LDFLAGS="-Wl,--no-keep-memory"
export CC=gcc
export CXX=g++
