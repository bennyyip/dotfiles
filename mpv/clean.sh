#!/bin/bash
set -eu
(
  cd ./.state/watch_later/
  fd -d 1 --changed-before 1y -X rm
)
