#!/bin/bash
set -e

# This script overwrites the linux symbol-table element in library/xml/symbols.xml
# that has the specified $DF_TYPE (e.g. STEAM) with the current symbols for the DF
# version $DF_VER installed in the directory specified by the CMAKE_INSTALL_PREFIX
# cmake build variable.
#
# Pre-requisites:
#
#   - the DFHack cmake build is configured to install into the DF directory
#   - exactly one symbol-table element exists for linux64 $DF_TYPE in symbols.xml
#   - ansifilter is installed on the host system
#
# Syntax:
#
#   ./symbols_gen_linux.sh <DF version> <DF type> [<DFHack build dir>]
#
# If not specified, the DFHack build dir defaults to "build".
#
# The script will act upon the symbols.xml in the same directory as the script,
# regardless of what the the CWD is when the script is run.

SCRIPT_DIR=`dirname $0`
DFHACK_SRC_DIR=`cd "${SCRIPT_DIR}/../.." && pwd`

. ${SCRIPT_DIR}/symbols_gen_common.sh

BUILD_DIR="${DFHACK_SRC_DIR}/${3:-build}"
INSTALL_PREFIX=`grep CMAKE_INSTALL_PREFIX:PATH= ${BUILD_DIR}/CMakeCache.txt | cut -d= -f2`
DF_DIR=`cd ${BUILD_DIR} && cd ${INSTALL_PREFIX} && pwd`
DWARFORT="${DF_DIR}/dwarfort"
ANSIFILTER="ansifilter"
DFHACK_RUN="${DF_DIR}/dfhack-run"

if [ ! -x "${DWARFORT}" ]; then
    echo "DF executable not found or not executable: ${DWARFORT}"
    exit 1
elif [ -z "$(which ${ANSIFILTER})" ]; then
    echo "required dependency not installed: ${ANSIFILTER}"
    exit 1
fi

df_hash=`md5sum "${DWARFORT}" | cut -d" " -f1`
hash_elem="<md5-hash value='${df_hash}'/>"

write_symbol_table linux64 linux "${hash_elem}" "" ""
(cd "${BUILD_DIR}" && ninja install)
(cd "${DF_DIR}" && DFHACK_DISABLE_CONSOLE=1 ./dfhack) &

offsets=''
while [ -z "${offsets}" ]; do
    sleep 1
    echo "waiting for DF to start"
    offsets=`"${DFHACK_RUN}" devel/dump-offsets | ${ANSIFILTER} | fgrep global-address`
done
vtables=`"${DFHACK_RUN}" devel/scan-vtables | ${ANSIFILTER} | LANG=C sort | fgrep vtable-address`
write_symbol_table linux64 linux "${hash_elem}" "$offsets" "$vtables"

"${DFHACK_RUN}" die
echo "done"
