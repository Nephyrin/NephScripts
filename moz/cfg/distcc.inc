if [[ -n $DISTCC_HOSTS ]]; then
  [[ -z $MAKE_JOBS ]] || MAKE_JOBS="$(distcc -j)"
  # Note that ccache needs to be disabled if you wish to use distcc-pump mode

  # non-ccache
  # ac_add_options --with-compiler-wrapper=/usr/bin/distcc
  # ccache
  ac_add_options --with-compiler-wrapper="/usr/bin/ccache /usr/bin/distcc"

  # Obsolete way of enabling distcc/ccache, ignored by newer builds so we can
  # keep both around:

  # non-ccache
  # MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS CC='distcc $CC' CXX='distcc $CXX'"
  # ccache
  ac_add_options --with-ccache
  MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS CCACHE_PREFIX=distcc"
fi

# This allows |MAKE_JOBS=10 ./mach ...|, and defaults to distcc -j
[[ -z $MAKE_JOBS ]] || mk_add_options MOZ_PARALLEL_BUILD="$MAKE_JOBS"
# for builds prior to MOZ_PARALLEL_BUILD, newer builds will ignore:
[[ -z $MAKE_JOBS ]] || MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS -j$MAKE_JOBS"

mk_add_options MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS"
