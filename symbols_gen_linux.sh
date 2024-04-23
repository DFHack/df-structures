#!/bin/bash

# This script overwrites the linux symbol-table element in library/xml/symbols.xml
# that has the specified $DF_TYPE with the current symbols for the DF version
# $DF_VER installed in the given directory. $DF_TYPE can be STEAM, ITCH, CLASSIC,
# or LOCAL.
#
# Pre-requisites:
#
#   - DFHack is already installed in the given DF directory
#   - exactly one symbol-table element exists for linux64 $DF_TYPE in symbols.xml
#   - ansifilter is installed on the host system
#
# Syntax:
#
#   path/to/library/xml/symbols_gen_linux.sh <DF version> <DF type> [<DF dir>]
#
# If the DF dir is not specified, it defaults to the location that cmake is
# configured to install DFHack to in the "../../build" dir cmake configuration.
# This default only works if this script is run from a configured DFHack source
# tree.
#
# The script will act upon the symbols.xml in the same directory as this script,
# regardless of what the the CWD is when this script is run.

SCRIPT_DIR=`dirname $0`

. ${SCRIPT_DIR}/symbols_gen_common.sh

if [ -z "${DF_DIR}" ]; then
    build_dir=`cd "${SCRIPT_DIR}/../../build" && pwd`
    if [ -n "${build_dir}" ]; then
        install_prefix=`grep CMAKE_INSTALL_PREFIX:PATH= "${build_dir}/CMakeCache.txt" | cut -d= -f2`
        DF_DIR=`cd "${build_dir}" && cd "${install_prefix}" && pwd`
    fi
fi

DWARFORT="${DF_DIR}/dwarfort"
ANSIFILTER="ansifilter"
DFHACK_RUN="${DF_DIR}/dfhack-run"

if [ ! -x "${DWARFORT}" ]; then
    echo "DF not found or not executable: ${DWARFORT}"
    exit 1
elif [ ! -x "${DFHACK_RUN}" ]; then
    echo "DFHack must be installed in ${DF_DIR}"
    exit 1
elif [ -z "$(which ${ANSIFILTER})" ]; then
    echo "required dependency not installed: ${ANSIFILTER}"
    exit 1
fi

df_hash=`md5sum "${DWARFORT}" | cut -d" " -f1`
hash_elem="<md5-hash value='${df_hash}'/>"

write_symbol_table linux64 linux "${hash_elem}" "" ""
cp "${SYMBOLS_XML}" "${DF_DIR}/hack/symbols.xml"
DFHACK_DISABLE_CONSOLE=1 "${DF_DIR}/dfhack" &

offsets=''
while [ -z "${offsets}" ]; do
    sleep 0.5
    echo "waiting for DF to start"
    offsets=`"${DFHACK_RUN}" devel/dump-offsets | ${ANSIFILTER} | fgrep global-address`
done
vtables=`"${DFHACK_RUN}" devel/scan-vtables | ${ANSIFILTER} | LANG=C sort | fgrep vtable-address`
write_symbol_table linux64 linux "${hash_elem}" "$offsets" "$vtables"

"${DFHACK_RUN}" die
echo "done"
