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

DFHACK_SRC_DIR=`cd "$(dirname ${0})/../.." && pwd`

DF_VER="${1}"
DF_TYPE="${2}"
BUILD_DIR="${DFHACK_SRC_DIR}/${3:-build}"

SYMBOLS_XML="${DFHACK_SRC_DIR}/library/xml/symbols.xml"
INSTALL_PREFIX=`grep CMAKE_INSTALL_PREFIX:PATH= ${BUILD_DIR}/CMakeCache.txt | cut -d= -f2`
DF_DIR=`cd ${BUILD_DIR} && cd ${INSTALL_PREFIX} && pwd`
DWARFORT="${DF_DIR}/dwarfort"
ANSIFILTER="ansifilter"
DFHACK_RUN="${DF_DIR}/dfhack-run"

if [ -z "${DF_VER}" ]; then
    echo "DF version (arg 1) must be specified"
    exit 1
elif [ -z "${DF_TYPE}" ]; then
    echo "DF distribution type (arg 2) must be specified"
    exit 1
elif [ ! -w "${SYMBOLS_XML}" ]; then
    echo "DFHack symbols not found or not writable: ${SYMBOLS_XML}"
    exit 1
elif [ ! -x "${DWARFORT}" ]; then
    echo "DF executable not found or not executable: ${DWARFORT}"
    exit 1
elif [ -z "$(which ${ANSIFILTER})" ]; then
    echo "required dependency not installed: ${ANSIFILTER}"
    exit 1
fi

df_hash=`md5sum "${DWARFORT}" | cut -d" " -f1`
offsets=''
vtables=''

write_symbol_table() {
    tmpfile=`mktemp`

    cat >${tmpfile} <<EOF
    <symbol-table name='v0.${DF_VER} linux64 ${DF_TYPE}' os-type='linux'>
        <md5-hash value='${df_hash}'/>

${offsets}

${vtables}
    </symbol-table>
EOF

    start_pattern="<symbol-table.*linux64 ${DF_TYPE}"
    end_pattern="<\/symbol-table>"
    sed -i "/${start_pattern}/,/${end_pattern}/{/${end_pattern}/{x;r ${tmpfile}
    D};d}" "${SYMBOLS_XML}"
    rm "${tmpfile}"
}

write_symbol_table
(cd "${BUILD_DIR}" && ninja install)
(cd "${DF_DIR}" && DFHACK_DISABLE_CONSOLE=1 ./dfhack) &

offsets=''
while [ -z "${offsets}" ]; do
    sleep 1
    echo "waiting for DF to start"
    offsets=`"${DFHACK_RUN}" devel/dump-offsets | ${ANSIFILTER} | fgrep global-address | sed "s/^/        /"`
done
vtables=`"${DFHACK_RUN}" devel/scan-vtables | ${ANSIFILTER} | LANG=C sort | fgrep vtable-address | sed "s/^/        /"`
write_symbol_table

"${DFHACK_RUN}" die
echo "done"
