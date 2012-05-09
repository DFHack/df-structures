#!/bin/bash

cd `dirname "$0"`

exec ./start.sh-core \
    --load start-utils.lisp --eval '(work::make-csv)' --eval '(quit)'
