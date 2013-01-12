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

./make-keybindings.pl < "$DFPATH/df_linux/g_src/keybindings.h" > df.keybindings.xml

LINUX_DF="$DFPATH/df_linux/libs/Dwarf_Fortress"

rscript scan_vtable.rb "$LINUX_DF" > linux/vtables.txt
rscript scan_vtable.rb --dumpfuncs "$LINUX_DF" > linux/vtables-ext.txt
rscript scan_nextid.rb "$LINUX_DF" > linux/nextid.txt
rscript scan_ctors.rb "$LINUX_DF" > linux/ctors.txt
rscript scan_standingorders.rb "$LINUX_DF" > linux/standingorders.txt

./match-ctors.pl linux/ctors.txt linux/ctors-base.txt > linux/cglobals.txt

WINDOWS_DF="$DFPATH/df_windows/Dwarf Fortress.exe"

rscript scan_vtable.rb "$WINDOWS_DF"  > windows/vtables.txt
rscript scan_vtable.rb --dumpfuncs "$WINDOWS_DF" > windows/vtables-ext.txt
rscript scan_keybinding.rb "$WINDOWS_DF" > windows/keydisplay.txt
rscript scan_nextid.rb "$WINDOWS_DF" > windows/nextid.txt
rscript scan_standingorders.rb "$WINDOWS_DF" > windows/standingorders.txt

OSX_DF="$DFPATH/df_osx/dwarfort.exe"

rscript scan_vtable.rb "$OSX_DF" > osx/vtables.txt
rscript scan_keybinding.rb "$OSX_DF" > osx/keydisplay.txt
rscript scan_nextid_osx.rb "$OSX_DF" > osx/nextid.txt
rscript scan_ctors_osx.rb "$OSX_DF" | \
  perl -pe 's/(<global-object )(.*)(name=".*" offset=".*" size=".*")\/>/$1$3>\n    <comment>$2<\/comment>\n<\/global-object>/' > osx/ctors.txt

./match-ctors.pl osx/ctors.txt osx/ctors-base.txt > osx/cglobals.txt
