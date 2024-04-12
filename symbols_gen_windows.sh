#!/bin/bash
set -e

# This script overwrites the windows symbol-table element in library/xml/symbols.xml
# that has the specified $DF_TYPE (e.g. STEAM) with the current symbols for the
# specified DF version $DF_VER executable.
#
# Pre-requisites:
#
#   - exactly one symbol-table element exists for win64 $DF_TYPE in symbols.xml
#   - the df_misc repo is checked out in the df_misc subdirectory of the current dir
#   - the metasm repo is checked out in the metasm subdirectory of the current dir
#   - pefile is installed or pefile.py is available in the pefile subdirectory of the current dir
#   - python and ruby are installed
#
# Syntax:
#
#   ./symbols_gen_windows.sh <DF version> <DF type> [<path to DF executable>]
#
# If not specified, the path to the DF executable defaults to:
#   .steam/bin32/steamapps/content/app_975370/depot_975372/Dwarf Fortress.exe
#
# The script will act upon the symbols.xml in the same directory as the script,
# regardless of what the the CWD is when the script is run.

SCRIPT_DIR=`dirname $0`
DFHACK_SRC_DIR=`cd "${SCRIPT_DIR}/../.." && pwd`

. ${SCRIPT_DIR}/symbols_gen_common.sh

DF_EXE="${3:-$HOME/.steam/bin32/steamapps/content/app_975370/depot_975372/Dwarf Fortress.exe}"

DF_MISC_DIR="$(pwd)/df_misc"
METASM_DIR="$(pwd)/metasm"

if which pefile >/dev/null 2>&1; then
    PEFILE=pefile
elif [ -r pefile/pefile.py ]; then
    PEFILE="cd pefile && python pefile.py"
fi

if [ ! -x "${DF_EXE}" ]; then
    echo "DF executable not found: ${DF_EXE}"
    exit 1
elif [ ! -d "${DF_MISC_DIR}" ]; then
    echo "required dependency not in subdir: df_misc"
    exit 1
elif [ ! -d "${METASM_DIR}" ]; then
    echo "required dependency not in subdir: metasm"
    exit 1
fi

pe_timestamp=`eval ${PEFILE} \"${DF_EXE}\" | fgrep TimeDateStamp | head -n1 | sed 's/  */ /g' | cut -d" " -f4`

timestamp_elem="<binary-timestamp value='${pe_timestamp}'/>"

offsets=`cd df_misc && RUBYLIB="${METASM_DIR}" ruby dump_df_globals.rb "${DF_EXE}"`
vtables=`cd df_misc && RUBYLIB="${METASM_DIR}" ruby scan_vtable.rb "${DF_EXE}"`

write_symbol_table win64 windows "${timestamp_elem}" "$offsets" "$vtables"
echo "done"
