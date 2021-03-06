# Based on http://hg.mozilla.org/build/buildbot-configs/file/tip/mozilla2/linux64/mozilla-central/debug/mozconfig

ac_add_options --enable-application=browser
ac_add_options --disable-optimize
ac_add_options --enable-debug
ac_add_options --disable-install-strip
ac_add_options --enable-tests
ac_add_options --enable-crashreporter

ac_add_options --enable-ipdl-tests
# Optional
ac_add_options --enable-profiling
# ac_add_options --enable-codesighs
# ac_add_options --enable-trace-malloc
# ac_add_options --enable-dmd
# ac_add_options --enable-js-diagnostics

# Disabled again for bug 1047494
# ac_add_options --enable-warnings-as-errors

# ac_add_options --disable-webrtc

# This seems to just include CC=/hardcoded/buildbot/path/gcc-4.5
# . $topsrcdir/build/unix/mozconfig.linux

# -Werror=unused-variable catches issues.
# export CFLAGS="$CFLAGS"
# export CXXFLAGS="$CXXFLAGS"

# For NSS symbols
export MOZ_DEBUG_SYMBOLS=1
ac_add_options --enable-debug-symbols="-ggdb"

# Needed to enable breakpad (pre bug 717538)
# export MOZILLA_OFFICIAL=1
# export MOZ_TELEMETRY_REPORTING=1

# Experimental build speedup.
export MOZ_PSEUDO_DERECURSE=1
