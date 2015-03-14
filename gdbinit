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


# original by Tavis Ormandy (http://my.opera.com/taviso/blog/index.dml/tag/gdb) (great fix!)
# modified to work with Mac OS X by fG!
# seems nasm shipping with Mac OS X has problems accepting input from stdin or heredoc
# input is read into a variable and sent to a temporary file which nasm can read
define assemble
  # dont enter routine again if user hits enter
  dont-repeat
  if ($argc)
    if (*$arg0 = *$arg0)
      # check if we have a valid address by dereferencing it,
      # if we havnt, this will cause the routine to exit.
    end
    printf "Instructions will be written to %#x.\n", $arg0
  else
    printf "Instructions will be written to stdout.\n"
  end
  printf "Type instructions, one per line."
  echo \033[1m
  printf " Do not forget to use NASM assembler syntax!\n"
  echo \033[0m
  printf "End with a line saying just \"end\".\n"
  if ($argc)
    # argument specified, assemble instructions into memory at address specified.
    shell ASMOPCODE="$(while read -ep '>' r && test "$r" != end ; do echo -E "$r"; done)" ; FILENAME=$RANDOM; \
    echo -e "BITS 32\n$ASMOPCODE" >/tmp/$FILENAME ; /usr/bin/nasm -f bin -o /dev/stdout /tmp/$FILENAME | /usr/bin/hexdump -ve '1/1 "set *((unsigned char *) $arg0 + %#2_ax) = %#02x\n"' >/tmp/gdbassemble ; /bin/rm -f /tmp/$FILENAME
    source /tmp/gdbassemble
    # all done. clean the temporary file
    shell /bin/rm -f /tmp/gdbassemble
  else
    # no argument, assemble instructions to stdout
    shell ASMOPCODE="$(while read -ep '>' r && test "$r" != end ; do echo -E "$r"; done)" ; FILENAME=$RANDOM; \
    echo -e "BITS 32\n$ASMOPCODE" >/tmp/$FILENAME ; /usr/bin/nasm -f bin -o /dev/stdout /tmp/$FILENAME | /usr/bin/ndisasm -i -b32 /dev/stdin ; /bin/rm -f /tmp/$FILENAME
  end
end
document assemble
Assemble instructions using nasm.
Type a line containing "end" to indicate the end.
If an address is specified, insert/modify instructions at that address.
If no address is specified, assembled instructions are printed to stdout.
Use the pseudo instruction "org ADDR" to set the base address.
end
