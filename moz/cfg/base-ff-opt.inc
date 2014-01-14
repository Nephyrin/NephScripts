ac_add_options --enable-application=browser
ac_add_options --enable-optimize
ac_add_options --disable-debug
ac_add_options --enable-tests
#ac_add_options --enable-codesighs

# . $topsrcdir/build/unix/mozconfig.linux

# Neph
# ac_add_options --enable-gstreamer
# Breaks gstreamer + clang-3.2
# ac_add_options --disable-warnings-as-errors

# ac_add_options --disable-elf-hack # Bug 771569
# export MOZ_FLEXBOX=1
# export MOZ_PER_WINDOW_PRIVATE_BROWSING=1
ac_add_options --enable-profiling

# Broken on new clang
# ac_add_options --disable-webrtc

# ac_add_options --enable-default-toolkit=cairo-qt
# ac_add_options --disable-installer
ac_add_options --disable-crashreporter
ac_add_options --disable-javaxpcom
# ac_add_options --disable-printing
# ac_add_options --disable-embedding-tests
ac_add_options --disable-install-strip

# Mozilla-Central nightlies only since this has a cost in performance
# ac_add_options --enable-js-diagnostics

# export CFLAGS="$CFLAGS -ggdb"
# export CXXFLAGS="$CXXFLAGS -ggdb"

# For NSS symbols
export MOZ_DEBUG_SYMBOLS=1
ac_add_options --enable-debug-symbols="-ggdb"

# Needed to enable breakpad in application.ini
# export MOZILLA_OFFICIAL=1

# export MOZ_TELEMETRY_REPORTING=1

# Experimental build speedup.
export MOZ_PSEUDO_DERECURSE=1