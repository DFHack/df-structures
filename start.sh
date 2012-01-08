#!/bin/bash
./sbcl-runtime --dynamic-space-size 1500 \
    --no-sysinit --no-userinit \
    --eval '(pushnew "cl-linux-debug/" asdf:*central-registry*)' \
    --load start.lisp --eval '(in-package :work)' \
    --eval '(browse $global.*)'
