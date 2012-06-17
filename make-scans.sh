#!/bin/bash

DFPATH=~/Games/DF

if [ -n "$1" ]; then
    DFPATH="$1"
fi

function rscript() {
    SCRIPT=$1
    shift
    echo "Running $SCRIPT..." 1>&2
    ruby -I $DFPATH/metasm "$DFPATH/df_misc/$SCRIPT" "$@"
}

rm -f */vtables.txt */nextid.txt

LINUX_DF="$DFPATH/df_linux/libs/Dwarf_Fortress"

rscript scan_linux_vtable.rb "$LINUX_DF" > linux/vtables.txt
rscript scan_linux_vtable.rb --dumpfuncs "$LINUX_DF" > linux/vtables-ext.txt
rscript scan_nextid.rb "$LINUX_DF" > linux/nextid.txt
rscript scan_ctors.rb "$LINUX_DF" > linux/ctors.txt
./match-ctors.pl linux/ctors.txt linux/ctors-base.txt > linux/cglobals.txt

rscript scan_win_vtable.rb "$DFPATH/df_windows/Dwarf Fortress.exe" > windows/vtables.txt
rscript scan_nextid.rb "$DFPATH/df_windows/Dwarf Fortress.exe" > windows/nextid.txt

rscript scan_nextid_osx.rb "$DFPATH/df_osx/dwarfort.exe" > osx/nextid.txt
