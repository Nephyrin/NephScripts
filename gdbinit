# Mark workdir as safe
add-auto-load-safe-path .
add-auto-load-safe-path /usr/lib/
add-auto-load-safe-path /usr/lib32/

set print pretty on
set print object on
set pagination off
set disassembly-flavor intel

# I want to not-care about SIGPIPE more often than I want to care
handle SIGPIPE nostop print pass

# Used by libdl apparently, rarely useful
handle SIG38 noprint nostop pass

set prompt [gdb] 
