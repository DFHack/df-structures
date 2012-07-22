################################
Data Structure Definition Syntax
################################

.. contents::

This document documents the XML syntax used to define
DF data structures for use in dfhack.


==================
General Background
==================

Originally dfhack used a file called ``Memory.xml``
to describe data structures of the game. It explicitly
listed addresses of known global variables, and offsets
within structures to fields, not unlike the ini files
used by Dwarf Therapist.

This format is a good choice when only a small number
of fields and objects need to be accessed, and allows
a program to work with many different versions of DF,
provided that the relevant fields and objects work
in the same way.

However, as the number of known fields and objects grow,
maintaining the explicit offset lists quickly becomes
difficult and error prone. Also, even when almost all
fields of a structure become known, the format fails to
represent and exploit their relative position, which in
practice is actually more stable than the specific offset
values.

This format instead represents data structure layout
purely via listing all fields in the correct order,
exactly like a structure definition does in the C++
language itself; in fact, these XML definitions are
translated into C++ headers in a mostly straightforward
way (the more tricky bits are things like correctly
processing circular references, or generating metadata
for lua). There is still a file with numeric data,
but it only contains absolute addresses of global
objects.

As a downside, dfhack now needs to be recompiled
every time layout of some data structure changes;
on the other hand, accessing DF structures from C++
plugins now has no overhead compared with DF's
own code. Also, practice shows that the more fields
are known in a structure, the easier it is to spot
what exactly has changed, and fix the exact area.


===============
XML file format
===============

All XML files use ``<data-definition>`` as their root tag.

They should be indented using 4 spaces per level, without tabs.

Unless noted otherwise, all non-root tags allow using a
*comment* attribute, or a ``<comment>...</comment>`` subtag.
It may be used to include a comment string that can be used
by tools processing the xml.

Excluding content of tags like ``<comment>`` or ``<code-helper>``,
all plain text inside tag bodies is ignored and may be freely
used instead of XML comments.

**NOTE:** Using XML tags and/or attributes not defined in this document
is not allowed.


Enum type definition
====================

Global enum types are defined as follows::

    <enum-type type-name='name' [base-type='int32_t']>
        <enum-item [name='key1'] [value='0']/>
        <enum-item [name='key2'] [value='1']/>
        ...
    </enum-type>

Every enum has an integer base type, which defaults to *int32_t* if omitted.

Like in C++, enum items may either explicitly specify an integer value, or
rely on auto-increment behavior.

**NOTE:** Due to a codegen limitation, specifying value on any item other
than the first one prevents using the attribute feature described below.

As in most cases, the *name* attribute may be omitted if unknown; the code
generator would produce a random identifier to satisfy C++ language requirements.


Enum item attributes
--------------------

The XML syntax allows associating attributes with enum items,
thus embedding lookup tables for use in C++ or lua code.

Every attribute must be declared at the top level of the enum::

    <enum-attr name='attr'
              [type-name='primitive-or-enum']
              [default-value='...']
              [use-key-name='true/false']
              [is-list='true/false']/>

The declaration allows specifying a numeric, or other enum type for the
attribute, overriding the default ``const char*`` string type.

An explicit default value may also be specified; otherwise the attribute
defaults to NULL or 0. If ``use-key-name`` is *true*, the corresponding
``enum-item``'s *name* is used as the default value.

Alternatively, an attribute may be declared to be a list, instead of a scalar.
In this case, the default is an empty list.

**NOTE:** Attribute name ``'key'`` is reserved for a built-in string attribute
representing the enum item key.

For every declared attribute, every enum-item tag may contain an attribute
value definition::

    <enum-item name='key'>
        <item-attr name='attr' value='...'/>
        ...
    </enum-item>

For list attributes, multiple ``item-attr`` entries may be used to define the
list contents.


Bitfield type definition
========================

Global bitfield types are defined as follows::

    <bitfield-type type-name='name' [base-type='uint32_t']>
        <flag-bit [name='bit1'] [count='1'] [type-name='enum']/>
        <flag-bit [name='bit2'] [count='1'] [type-name='enum']/>
        ...
    </bitfield-type>

Like enums, bitfields have an integer base type, which defaults to *uint32_t*.
The total number of bits in the bitfield must not exceed the base type size.

A bitfield item may be defined to occupy multiple bits via the *count* attribute.
It also may have an enum type; due to compiler limitations, the base-type of the
enum must be exactly the same as the bitfield itself.


Structure type definition
=========================

Structures without virtual methods are defined as follows::

    <struct-type type-name='name'
                [is-union='true/false']
                [inherits-from='struct_type']
                [instance-vector='expr']
                [key-field='identifier']>
        ...
        fields
        ...
    </struct-type>

The *instance-vector* attribute may be used to specify a global
vector that canonically contains all instances of the structure.
Code generation uses it to produce a ``find`` static method.
If *key-field* is specified, this method uses binary search
by the referred field; otherwise it just indexes the vector
with its integer argument.


Common field properties
-----------------------

All fields support the following attributes:

``name``
    Specifies the identifier naming the field.

    This attribute may be omitted, in which case
    the code generator produces a random identifier. As
    follows from the word random, such identifiers aren't
    stable, and shouldn't be used to access the field.

``offset``, ``size``, ``alignment``
    Specifies the offset, size and alignment in bytes.

    **WARNING:** Although allowed for any field by the XML syntax,
    and supported by the lisp GUI tool, code generation will fail
    with these attributes except in cases specifically shown below.

    With the above caveat, ``size`` and ``alignment`` may also
    be used on the ``struct-type`` tag itself.


Primitive fields
----------------

Primitive fields can be classified as following:

1)  Unmarked area::

        <padding name='id' size='bytes' [alignment='1/2/4'] .../>

    This tag defines an area of raw bytes with unknown contents.

2)  Numbers::

        <int32_t name='id'.../>

    Supported number types are: ``int8_t``, ``uint8_t``, ``int16_t``,
    ``uint16_t``, ``int32_t``, ``uint32_t``, ``int64_t``, ``uint64_t``,
    ``s-float`` (single float).

3)  Boolean::

        <bool name='id'.../>

4)  String::

        <static-string name='id' size='bytes'.../>
        <ptr-string name='id'.../>
        <stl-string name='id'.../>

    These tags correspond to ``char[bytes]``, ``char*``, and ``std::string``.

Primitives support the following attributes:

``refers-to='expr'``

    Specifies a GUI hyperlink to an object returned by an arbitrary expression.

    The expression receives the value of the field as ``$``, and the reference
    to the field as ``$$``.

``ref-target='type'``

    Specifies a hyperlink to an instance of *type*, identified by the value of the field.
    The instance is retrieved via *instance-vector* and *key-field*, or
    a ``<code-helper name='find-instance'>`` in the target type definition.

``aux-value='expr'``

    Specifies an additional value for use in the *find-instance* code helper.

    Unlike *refers-to*, the expression receives the **reference** to the field
    as ``$``, and a reference to the containing structure as ``$$``; i.e. the
    arguments are shifted one step toward parent. This is because the value
    of the field is already implicitly passed to *find-instance*.

    The *find-instance* helper receives the field value as ``$``, and aux-value as ``$$``.


Substructure fields
-------------------

Nested structures are defined via the ``compound`` tag::

    <compound name='id' type-name='struct_type'/>

    <compound [name='id'] [is-union='true/false'] [key-field='id']>
        ...
        field
        ...
    </compound>

As seen above, a nested structure may either use a global type
defined elsewhere, or define an ad-hoc structure in-place.
In the in-place case, omitting *name* has a special meaning
of defining an anonymous nested struct or union.


Enum fields
-----------

Fields of enum types are defined as follows::

    <enum name='id' type-name='enum_type' [base-type='int32_t']/>

    <enum name='id' [base-type='int32_t']>
        <enum-item name='key1'.../>
        ...
    </enum>

Like with substructures, enums may be either referenced globals, or ad-hoc definitions.

In the former case, when *base-type* of the field and the enum differ,
a special wrapper is added to coerce the size, or, if impossible,
the enum type is completely replaced with the *base-type*. The net
effect is that the field *always* has the expected size and alignment.

If no *base-type* is specified on the field, the one in the global type
definition has complete precedence. This is not recommended.


Nested bitfields
----------------

Ad-hoc bitfields are defined as follows::

    <bitfield name='id' [base-type='uint32_t']>
        <flag-bit name='key1'.../>
        ...
    </bitfield>

In order to reference a global type, use ``<compound>``.


Container fields
----------------

A number of tags fall under the 'container' abstraction.
The common element is that the fields they define reference
objects of another type. This includes things like pointers,
arrays or vectors.

Abstract container
..................

The basic syntactic property of a container is that it requires
exactly one nested field tag in order to specify the contained item::

    <container>
        <field .../>
    </container>

**NOTE:** The ``container`` tag is used here as a placeholder for any real
tag following the container syntax.

For convenience, the following automatic rewrite rules are applied:

1)  The ``type-name`` attribute::

        <container type-name='foo' .../>

    is rewritten into::

        <container ...>
            <compound type-name='foo' .../>
        </container>

    or, if *foo* is a primitive type::

        <container ...>
            <foo .../>
        </container>

2)  The ``pointer-type`` attribute::

        <container pointer-type='foo' .../>

    is rewritten into::

        <container ...>
            <pointer type-name='foo' .../>
        </container>

3)  Multiple nested fields::

        <container ...>
            <field1 .../>
            <field2 .../>
        </container>

    are aggregated together::

        <container ...>
            <compound ...>
                <field1 .../>
                <field2 .../>
            </compound>
        </container>

4)  If no item is specified, ``padding`` is assumed::

        <container>
            <padding size='4'/>
        </container>

**NOTE:** These rules are mutually exclusive, and it is an error
to specify both of the attributes (unless it is ``type-name='pointer'``),
or combine nested fields with any of them.

When the above rewrites are applied and result in creation of a new tag,
the following attributes are copied to it from the container tag, if
applicable: ``key-field``, ``refers-to``, ``ref-target``, ``aux-value``.
They otherwise have no effect on the container itself.

This means that::

    <container pointer-type='int32_t' ref-target='foo'/>

eventually rewrites to::

    <container pointer-type='int32_t' ref-target='foo'>
        <pointer type-name='int32_t' ref-target='foo'>
            <int32_t ref-target='foo'/>
        </pointer>
    </container>

Abstract containers allow the following attributes:

``has-bad-pointers='true'``

    Tells the GUI tool to ignore this field in some of its memory
    scans, because this container may contain invalid pointers,
    which can confuse the analysis code.

Pointer fields
..............

As seen above, the ``pointer`` tag is a subtype of abstract container.

If the pointer refers to an array of objects, instead of one instance,
the *is-array* attribute should be used:

    <pointer type-name='foo' is-array='true'/>

Currently this attribute is ignored by C++ code generation, but
the GUI tool properly displays such fields as arrays.

Abstract sequence
.................

Containers that actually contain a sequence of objects support these
additional attributes:

``index-refers-to='expr'``

    Specifies a GUI hyperlink from any item in the container to the
    object returned by the expression.

    The expression receives the index of the item in the container
    as ``$``, and a reference to the container as ``$$``.

``index-enum='enum_type'``

    Associates an enum with the indices of the container. The GUI
    tries to use enum item names instead of numbers when displaying
    the items, and lua may allow using strings as indices.

Standard containers
...................

``<static-array name='id' count='123' .../>``

    Defines a simple C++ array of the specified length.

``<stl-vector name='id'.../>``

    Defines an ``std::vector<item>`` field.

``<stl-deque name='id'.../>``

    Defines an ``std::deque<item>`` field.

``<stl-bit-vector name='id'.../>``

    Defines an ``std::vector<bool>`` field.

    STL defines ``vector<bool>`` as a special type that actually contains bits.
    These XML definitions use a separate tag for it; ``<stl-vector type-name='bool'/>``
    is rendered into C++ as ``vector<char>``.

DF-specific containers
......................

These are defined in df-code.lisp:

``<df-flagarray name='id' index-enum='enum'/>``

    Defines a ``BitArray<enum>`` field.

``<df-array name='id' .../>``

    Defines a ``DfArray<item>`` field.

``<df-linked-list name='id' type-name='foo_link'/>``

    Defines an ad-hoc DF-style linked list. In C++ actually equivalent to::

        <compound type-name='foo_link'/>

    but allows the GUI to display it as a list.


Class type definition
=====================

In the context of these XML definitions, class denotes types with virtual methods::

    <class-type type-name='name'
               [inherits-from='class_type']
               [original-name='vtable_name']
               ...>
        ...
        fields
        ...
        <virtual-methods>
            ...
            vmethods
            ...
        </virtual-methods>
    </class-type>

Classes are generally the same as ``<struct-type>``, including support for *instance-vector*.
Unlike ``<struct-type>`` however, they don't allow ``is-union='true'``.

There may only be one table of virtual methods per class-type. In subclasses it
should only contain items added to the table of the superclass.


Virtual method definition
-------------------------

Virtual method definitions are placed within the ``<virtual-methods>``
section of a class type. No other tag may be placed within that section,
including *comment*.

A virtual destructor is defined as follows::

    <vmethod is-destructor='true'/>

Ordinary virtual methods use the following syntax::

    <vmethod [name='id'] [ret-type='type']>
        [<ret-type .../>]
        <field1.../>
        <field2.../>
        ...
    </vmethod>

The return type may be specified either as an attribute, or via a ``ret-type`` sub-tag.
The subtag syntax follows the abstract container model outlined above. The attribute is
exactly equivalent to ``<ret-type type-name='type'/>`` as subtag. If the return type is
completely omitted, it is taken to be void.

Ordinary field definition tags within the vmethod tag are treated as method parameters.

If the *name* attribute is omitted, the vmethod is named randomly and made protected,
so that calling it is impossible. This is the intended way of providing placeholders
for completely unknown slots in the vtable.


Global object definition
========================

Global objects are global pointers that are initialized from symbols.xml at runtime.
Therefore, the tag itself is identical in syntax to ``<pointer>``, except that it
doesn't allow *is-array*::

    <global-object name='id' type-name='...'/>

    <global-object name='id'>
        <field.../>
    </global-object>

C++ generation places them in the ``df::global`` namespace.

The *offset* attribute of the ``global-object`` tag represents the absolute
address. As noted above, it may only be used in files intended for the GUI.


Symbol table definition
=======================

Symbol tables are defined in symbols.xml and loaded at runtime.
They define locations of global objects and virtual tables.

The definition syntax is as follows::

    <symbol-table name='...' os-type='...'>
        <md5-hash value='...'/>
        <binary-timestamp value='0x...'/>
        ...

        <global-address name='...' [value='0x...']/>
        ...

        <vtable-address name='...' [value='0x...']/>
        ...
    </symbol-table>

The *name* attribute specifies an unique name of the symbol table.
*os-type* specifies the applicable OS type, and must be one of
``windows``, ``linux``, ``darwin``.

The ``<md5-hash>`` tag specifies the MD5 hash that is used to match
the executable on Linux and OS/X. It will be ignored if used in a
windows symbol table. Likewise, ``<binary-timestamp>`` is valid only
for matching EXE files. A symbol table may contain multiple tags
in order to match several executables; this is especially useful with
MD5 hashes, which change with patching.

Global object addresses are specified with ``<global-address>`` tags.
Virtual method table addresses may be pre-initialized with ``<vtable-address>`` tags.

It is allowed to specify addresses for objects and vtables that are otherwise
not defined. Obviously, such values can only be used by directly quering the
VersionInfo object in dfhack.
