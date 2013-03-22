
##
## Environment stuff.
##

# This stuff needs to be set in your environment. e.g.:
# - Put it in .bashrc OR
# - OR put it in ~/distcc.sh and run |source ~/distcc.sh| to use it
# - OR put it directly in a MOZCONFIG with:
#    mk_add_options MOZ_MAKE_FLAGS="DISTCC_HOSTS='...' CCACHE_PREFIX=distcc ..."

# Note if not using ccache your mozconfig needs additional tweaking, see below
export CCACHE_PREFIX=distcc

# --randomize means spread jobs out evenly over hosts.
# --localslots=4 means only fall back to four local jobs for files that can't be
#                compiled remotely.
# --localslots_cpp=16 means allow up to sixteen local preprocessing jobs
# /16 means send up to 16 jobs to that host, the values here are the max those
# hosts allow.

# NOTE: including localhost/8 will also do some compiling locally, but this will
# usually be *slow*, as with 32+ remote jobs available, the localhost is usually
# saturated just doing pre-processing to keep the distcc machines busy.
export DISTCC_HOSTS="--randomize
                     --localslots=4 --localslots_cpp=16
                     bangles.mv.mozilla.com/16
                     gandalf.mv.mozilla.com/16"

# Retry aggressively when no machines are available (default is 1000)
export DISTCC_PAUSE_TIME_MSEC=10

# This is exported for the mozconfig to use. |distcc -j| looks at the vars we
# just set and determines how many distcc jobs can run at once.
export MAKE_JOBS=$(( $(distcc -j) * 2 ))

##
## MOZCONFIG stuff
##

# OS X users need to add this to their CC/CXX vars:
#  export CC="clang -target x86_64-apple-darwin"
#  export CXX="clang++ -target x86_64-apple-darwin"

# If distcc is enabled, use the MAKE_JOBS we set, otherwise use a default of 8
if [ ! -z "$DISTCC_HOSTS" ]; then
    mk_add_options MOZ_MAKE_FLAGS="-j$MAKE_JOBS"
    # If NOT using ccache / CCACHE_PREFIX:
    #  mk_add_options MOZ_MAKE_FLAGS="-j$MAKE_JOBS CC='distcc $CC' CXX='distcc $CXX'"
else
    mk_add_options MOZ_MAKE_FLAGS="-j8"
fi
