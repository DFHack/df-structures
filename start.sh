#!/bin/bash
exec $0-core \
    --load start.lisp --eval '(in-package :work)' "$@"
