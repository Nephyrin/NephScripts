#!/usr/bin/env python

import struct
import sys
import os
import io

def die(msg):
    sys.stderr.write("!! %s\n" % (msg,))
    sys.exit(1)

def stat(msg):
    sys.stderr.write("%s\n" % (msg,))

if len(sys.argv) != 3:
    die("Requires two arguments: input, output")

inputfile = sys.argv[1]
outputfile = sys.argv[2]

stat("rpm2cpiogz - converting '%s' to '%s'\n" % (inputfile, outputfile if outputfile is not "-" else "STDOUT"))

ih = open(inputfile, "rb")

try:
    rpmlead = ih.read(96)
except IOError:
    die("Failed to read input file")

if len(rpmlead) != 96:
    die("This does not appear to be a valid RPM file (file length %u is shorter than a standard RPM header of 96)")

# See http://rpm5.org/docs/api/pkgformat.html
parse = struct.unpack("!BBBBBBhh66shh16B", rpmlead)
magic = parse[0:4]
version = parse[4:6]
rpmtype = parse[6]
archnum = parse[7]
rpmname = parse[8]
osnum = parse[9]
signature_type = parse[10]
reserved = parse[11:]

if magic != (0xed, 0xab, 0xee, 0xdb):
    die("This does not appear to be an rpm file")

try:
    rpmname = str(rpmname, encoding='utf-8')
    rpmname = rpmname[:rpmname.index('\x00')]
except (ValueError, UnicodeDecodeError):
    sys.stderr.write("ERR: RPM Name is bogus")
    rpmname = "<corrupt>"

stat("RPM Info:\n"
      "  Name:\n    %s\n"
      "  Version:\n    v%u.%u (signature v%i)\n"
      "  Type:\n    %i\n"
      "  Architecture:\n    %i\n"
      "  OS:\n    %i\n"
      "  Reserved bytes:\n    %s" %
      (rpmname, version[0], version[1], signature_type, rpmtype, archnum, osnum,
       ' '.join('0x%.2X' % x for x in reserved)))

if version != (3,0) or signature_type != 5:
    die("This tool only supports RPM v3.0 with signature type 5")

def readheader(hname, pad):
    hdat = ih.read(16)
    if len(hdat) != 16:
        die("Corrupt RPM, %s header has only %u bytes (expected minimum 16)" % (hname, len(hdat)))
    hparse = struct.unpack("!BBBBiii", hdat)
    hmagic = hparse[0:3]
    hver = hparse[3]
    hentries = hparse[5]
    hbytes = hparse[6]
    if hmagic != (0x8E, 0xAD, 0xE8):
        die("%s is not a valid RPM header struct (%s)" % (hname, ' '.join('0x%.2X' % x for x in hmagic)))
    stat("%s is v%u with %i entries (%i bytes)" % (hname, hver, hentries, hbytes))
    for x in range(hentries):
        edat = ih.read(16)
        eparse = struct.unpack("!iiii", edat)
        stat("  ENTRY tag %i, type %i, offset %i, count %i" % eparse)
    ih.read(hbytes)
    if pad and hbytes % 8:
        ih.read(8 - (hbytes % 8))
    #ih.seek(hbytes - 16, io.SEEK_CUR)

readheader("SIGNATURE", True)
readheader("RPMHEADER", False)

if outputfile is "-":
    oh = os.fdopen(sys.stdout.fileno(), "wb")
else:
    oh = open(outputfile, "wb")

oh.write(ih.read(-1))
