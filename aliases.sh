#!/bin/bash

pic() { x gwenview "$@"; }
x() {
  [ ! -z "$NEPH_CGROUP" ] && echo >&2 ":: WARNING: In cgroup"
  ("$@" >/dev/null 2>/dev/null &)
}
