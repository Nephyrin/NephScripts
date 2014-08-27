ac_add_options --enable-application=browser
ac_add_options --enable-optimize
ac_add_options --disable-debug
ac_add_options --enable-tests

# . $topsrcdir/build/unix/mozconfig.linux

# ac_add_options --enable-profiling

# ac_add_options --disable-installer
# ac_add_options --disable-crashreporter

# ac_add_options --disable-printing
# ac_add_options --disable-embedding-tests
ac_add_options --disable-install-strip

# Mozilla-Central nightlies only since this has a cost in performance
# ac_add_options --enable-js-diagnostics

# For NSS symbols
export MOZ_DEBUG_SYMBOLS=1

ac_add_options --enable-debug-symbols="-ggdb"

# Needed to enable breakpad in application.ini
# export MOZILLA_OFFICIAL=1

# export MOZ_TELEMETRY_REPORTING=1

# Experimental build speedup.
export MOZ_PSEUDO_DERECURSE=1
