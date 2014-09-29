export CC="clang"
export CXX="clang++"
# inline-new-delete can be removed when Bug 928808 lands
# https://bugzilla.mozilla.org/show_bug.cgi?id=928808
_cflags="-fcolor-diagnostics -Wno-inline-new-delete"

# This doesn't enable gold early enough in the process, so -flto fails because
# configure tries to link LLVM .o files with BFD ld.
# Hence the cfg/goldpath/ld -> /usr/bin/ld.gold hack. Adjust symlink as needed.
ac_add_options --enable-gold
_goldpath="-B$topsrcdir/../cfg/goldpath"

export LDFLAGS="$LDFLAGS $_goldpath"
export CFLAGS="$CFLAGS $_cflags $_goldpath"
export CXXFLAGS="$CXXFLAGS $_cflags $_goldpath"
#                        -Wl,--no-map-whole-files \
#                        -Wl,--no-keep-files-mapped \
#                        -Wl,--threads"
