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

rscript scan_linux_vtable.rb "$DFPATH/df_linux/libs/Dwarf_Fortress" > linux/vtables.txt
rscript scan_linux_vtable.rb --dumpfuncs "$DFPATH/df_linux/libs/Dwarf_Fortress" > linux/vtables-ext.txt
rscript scan_win_vtable.rb "$DFPATH/df_windows/Dwarf Fortress.exe" > windows/vtables.txt

rscript scan_nextid.rb "$DFPATH/df_linux/libs/Dwarf_Fortress" > linux/nextid.txt
rscript scan_nextid.rb "$DFPATH/df_windows/Dwarf Fortress.exe" > windows/nextid.txt
rscript scan_nextid_osx.rb "$DFPATH/df_osx/dwarfort.exe" > osx/nextid.txt
