#!/bin/bash
set -e

# This script is intended to be sourced from the other symbols_gen scripts.
# DFHACK_SRC_DIR must be defined before sourcing this script.

DF_VER="$1"
DF_TYPE="$2"

SYMBOLS_XML="${DFHACK_SRC_DIR}/library/xml/symbols.xml"

if [ -z "${DF_VER}" ]; then
    echo "DF version (arg 1) must be specified"
    exit 1
elif [ -z "${DF_TYPE}" ]; then
    echo "DF distribution type (arg 2) must be specified"
    exit 1
elif [ ! -w "${SYMBOLS_XML}" ]; then
    echo "DFHack symbols not found or not writable: ${SYMBOLS_XML}"
    exit 1
fi

write_symbol_table() {
    platform="$1"
    os_type="$2"
    id_elem="$3"
    offsets="$4"
    vtables="$5"

    tmpfile=`mktemp`

    formatted_offsets=`echo "${offsets}" | sed "s/^/        /"`
    formatted_vtables=`echo "${vtables}" | sed "s/^/        /"`

    cat >${tmpfile} <<EOF
    <symbol-table name='v0.${DF_VER} ${platform} ${DF_TYPE}' os-type='${os_type}'>
        ${id_elem}

${formatted_offsets}

${formatted_vtables}
    </symbol-table>
EOF

    start_pattern="<symbol-table.*${platform} ${DF_TYPE}"
    end_pattern="<\/symbol-table>"
    sed -i "/${start_pattern}/,/${end_pattern}/{/${end_pattern}/{x;r ${tmpfile}
    D};d}" "${SYMBOLS_XML}"
    rm "${tmpfile}"
}
