#!/bin/bash

cd `dirname "$0"`

./start.sh-core \
    --load start-utils.lisp --eval '(work::make-csv)' --eval '(quit)'

./make-dt.pl
