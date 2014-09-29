if [[ -n $DISTCC_HOSTS ]]; then
  [[ -n $MAKE_JOBS ]] || MAKE_JOBS="$(( $(distcc -j) * 2 ))"
  # Note that ccache needs to be disabled if you wish to use distcc-pump mode
  ac_add_options --with-compiler-wrapper="/usr/bin/ccache /usr/bin/distcc"
else
  ac_add_options --with-compiler-wrapper=/usr/bin/ccache
fi

# Obsolete way of enabling distcc/ccache, for older trees
# ac_add_options --with-ccache
# MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS CCACHE_PREFIX=distcc"
# MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS CC='distcc $CC' CXX='distcc $CXX'"

# This allows |MAKE_JOBS=10 ./mach ...|, and defaults to distcc -j
[[ -z $MAKE_JOBS ]] || mk_add_options MOZ_PARALLEL_BUILD="$MAKE_JOBS"
# for builds prior to MOZ_PARALLEL_BUILD, newer builds will ignore:
[[ -z $MAKE_JOBS ]] || MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS -j$MAKE_JOBS"

mk_add_options MOZ_MAKE_FLAGS="$MOZ_MAKE_FLAGS"
