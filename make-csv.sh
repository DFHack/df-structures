#!/bin/bash

cd $(dirname "$0")

./start.sh-core \
    --load start-utils.lisp --eval '(work::make-csv)' --eval '(quit)'

echo "Checking linux vtables:"
(cd linux; ../match-vtables.pl > all-vmethods.txt)
echo "Checking windows vtables:"
(cd windows; ../match-vtables.pl > all-vmethods.txt)

./make-dt.pl
