# DF-structures

DF-structures is the core information on which
[DFHack](https://github.com/DFHack/dfhack) depends.

For information on how to understand this format,
see [./SYNTAX.rst](./SYNTAX.rst).  For how to update
for a new version of DF, see
[./how-to-update.rst](./how-to-update.rst)

Originally dfhack used a file called ``Memory.xml``
to describe data structures of the game. It explicitly
listed addresses of known global variables, and offsets
within structures to fields, not unlike the ini files
used by Dwarf Therapist.

However, as the number of known fields and objects grow,
maintaining the explicit offset lists quickly becomes
difficult, error prone, and inefficient.

This format instead represents data structure layout
purely via listing all fields in the correct order,
exactly like a structure definition does in the C++
language itself. There is still a file with numeric data,
but it only contains absolute addresses of global objects.
