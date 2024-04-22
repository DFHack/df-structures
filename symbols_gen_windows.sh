#!/bin/bash

# This script overwrites the windows symbol-table element in library/xml/symbols.xml
# that has the specified $DF_TYPE with the current symbols for the DF version $DF_VER
# executable installed in the given DF dir. $DF_TYPE can be STEAM, ITCH, CLASSIC, or
# LOCAL.
#
# Pre-requisites:
#
#   - exactly one symbol-table element exists for win64 $DF_TYPE in symbols.xml
#   - python and ruby are installed
#   - the df_misc repo is checked out in the df_misc subdirectory of the current dir
#     - git clone --depth=1 https://github.com/DFHack/df_misc.git
#   - the metasm repo is checked out in the metasm subdirectory of the current dir
#     - git clone --depth=1 https://github.com/jjyg/metasm.git
#   - pefile is installed via pip or pefile.py is available in the pefile subdirectory of the current dir
#     - wget 'https://github.com/erocarrera/pefile/releases/download/v2023.2.7/pefile-2023.2.7.tar.gz
#     - mkdir pefile && tar xzf pefile-2023.2.7.tar.gz -C pefile --strip-components=1
#
# Syntax:
#
#   path/to/library/xml/symbols_gen_windows.sh <DF version> <DF type> [<DF dir>]
#
# If not specified, the DF dir defaults to:
#   $HOME/.steam/bin32/steamapps/content/app_975370/depot_975372
# which is where it shows up when the Windows binary manifest is downloaded with the
# Steam client. The only file that must be in this directory is Dwarf Fortress.exe.
#
# The script will act upon the symbols.xml in the same directory as this script,
# regardless of what the the CWD is when this script is run.

SCRIPT_DIR=`dirname $0`

. ${SCRIPT_DIR}/symbols_gen_common.sh

DF_DIR=${DF_DIR:-$HOME/.steam/bin32/steamapps/content/app_975370/depot_975372}
DF_EXE="${DF_DIR}/Dwarf Fortress.exe"

DF_MISC_DIR="$(pwd)/df_misc"
METASM_DIR="$(pwd)/metasm"

if [ -r pefile/pefile.py ]; then
    PE_DIR="pefile"
else
    PE_DIR=`pip show pefile | fgrep Location | sed 's/.*Location: //'`
fi

if [ ! -r "${DF_EXE}" ]; then
    echo "DF executable not found: ${DF_EXE}"
    exit 1
elif [ ! -d "${DF_MISC_DIR}" ]; then
    echo "required dependency not in subdir: df_misc"
    exit 1
elif [ ! -d "${METASM_DIR}" ]; then
    echo "required dependency not in subdir: metasm"
    exit 1
fi

pe_timestamp=`python3 "${PE_DIR}/pefile.py" "${DF_EXE}" | fgrep TimeDateStamp | head -n1 | sed 's/  */ /g' | cut -d" " -f4`

timestamp_elem="<binary-timestamp value='${pe_timestamp}'/>"

offsets=`RUBYLIB="${METASM_DIR}" ruby df_misc/dump_df_globals.rb "${DF_EXE}"`
vtables=`RUBYLIB="${METASM_DIR}" ruby df_misc/scan_vtable.rb "${DF_EXE}"`

write_symbol_table win64 windows "${timestamp_elem}" "$offsets" "$vtables"
echo "done"
