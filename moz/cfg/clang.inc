export CC="clang"
export CXX="clang++"
ac_add_options --enable-gold
# inline-new-delete can be removed when Bug 928808 lands
# https://bugzilla.mozilla.org/show_bug.cgi?id=928808
export CFLAGS="$CFLAGS -fcolor-diagnostics -Wno-inline-new-delete -B$topsrcdir/../cfg/goldpath/"
export CXXFLAGS="$CXXFLAGS -fcolor-diagnostics -Wno-inline-new-delete -B$topsrcdir/../cfg/goldpath/"
#export LDFLAGS="$LDFLAGS -Wl,--no-map-whole-files \
#                         -Wl,--no-keep-files-mapped \
#                         -Wl,--threads"
