
# This allows |MAKE_JOBS=10 ./mach ...|, and defaults to num_cpus*1.5
if [ -z "$MAKE_JOBS" ]; then
   _CPUS="$(egrep "^processor\s*:\s*[0-9]+$" /proc/cpuinfo | wc -l)"
   if [ ! -z "$_CPUS" ]; then
     MAKE_JOBS=$(( _CPUS * 15 / 10 ))
   else
     # Mach will also try to figure out num_cpus, direct make will just end up
     # with -j1 (I think)
     echo >&2 '!! '"Couldn't determine number of cores, and no MAKE_JOBS set"
   fi
fi

[ -z "$MAKE_JOBS" ] || mk_add_options MOZ_PARALLEL_BUILD="$MAKE_JOBS"
# for builds prior to MOZ_PARALLEL_BUILD, newer builds will ignore:
[ -z "$MAKE_JOBS" ] || MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS -j$MAKE_JOBS"

if [ ! -z "$DISTCC_HOSTS" ]; then
  # Note that ccache needs to be disabled if you wish to use distcc-pump mode

  # non-ccache
  ac_add_options --with-compiler-wrapper=/usr/bin/distcc
  # ccache
  # ac_add_options --with-compiler-wrapper="/usr/bin/ccache /usr/bin/distcc"

  # Obsolete way of enabling distcc/ccache, ignored by newer builds so we can
  # keep both around:

  # non-ccache
  MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS CC='distcc $CC' CXX='distcc $CXX'"
  # ccache
  # ac_add_options --with-ccache
  # MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS CCACHE_PREFIX=distcc"
fi

mk_add_options MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS"
