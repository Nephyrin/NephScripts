export CC="clang"
export CXX="clang++"
# inline-new-delete can be removed when Bug 928808 lands
# https://bugzilla.mozilla.org/show_bug.cgi?id=928808
export CFLAGS="$CFLAGS -fcolor-diagnostics -Wno-inline-new-delete"
export CXXFLAGS="$CXXFLAGS -fcolor-diagnostics -Wno-inline-new-delete"
#export LDFLAGS="$LDFLAGS -Wl,--no-map-whole-files \
#                         -Wl,--no-keep-files-mapped \
#                         -Wl,--threads"
