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

``init-value``
    Specifies the value that should be assigned to
    the field by the constructor. By default the following
    values are used:

    * For enums: the first element of the enum.
    * For signed integer fields with ``ref-target`` or ``refers-to``: -1.
    * For other numeric fields, pointers and bitfields: 0.

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
    ``s-float`` (single float), ``d-float`` (double float).

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

``<stl-set name='id'.../>``

    Defines an ``std::set<item>`` field.

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

``<df-static-flagarray name='id' index-enum='enum' count='numbytes'/>``

    Defines a ``StaticBitArray<numbytes,enum>`` field.

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

================
Lisp Integration
================

This XML file format was designed together with the ``cl-linux-debug``
Lisp tool, and has a number of aspects that closely integrate with
its internals.

For instance, when loaded by that tool, all XML tags are converted
directly into instances of classes that exactly match the name of
the tag, and when the documentation above mentions expressions, that
refers to Lisp expressions within the context of that library.

Reference expressions
=====================

In order to facilitate compact representation for long chains of
dereferences that are commonly required when dealing with the data
structures, ``cl-linux-debug`` defines a reader macro (i.e. basically
a parser plugin) that adds a custom syntax for them. This syntax is
triggered by special characters ``$`` and ``@``.

Expressions written in that syntax expand into nested chains of
calls to two generic functions named ``$`` and ``@``, which implement
correspondingly r-value and l-value dereference of their first
argument using the second.

Dereference syntax
------------------

The reader macro understands the following syntactic patterns:

* ``@``, ``$``, ``$$``, ``$$$``, ...

  Lone ``@`` and sequences of ``$`` are parsed just as the ordinary lisp
  parser would. This allows referring to the ``$`` and ``@`` functions,
  and using sequences of ``$`` characters as implicit argument names.

* ``$foo``

  A case-sensitive identifier preceeded by the ``$`` character
  is interned in the ``cl-linux-debug.field-names`` package as-is,
  and returned as the parsing result. The identifier may consist
  of letters, numbers, and ``-`` or ``_`` characters.

  The symbol is exported from its package and defined as a symbol
  macro expanding to ``'$foo``, and thus behaves as a case-sensitive
  keyword (which however can be used as a lexical variable name).
  All field & type names and other identifiers in the XML definitions
  are loaded into memory as such symbols.

* ``$foo:bar``

  This expands into ``'($foo . $bar)``; such pairs of identifiers
  are used in some special contexts.

* ``$foo.bar``, ``@foo.bar``

  These expressions expand to correspondingly ``($ foo '$bar)`` and
  ``(@ foo '$bar)``, representing thus r-value or l-value dereference
  of variable foo with literal key ``$bar``.

  The name ``foo`` may only contain characters listed above, but is
  otherwise separated and parsed with the regular lisp parser.

* ``$foo.*``, ``$foo[*]``, ``$foo.@``, ``$foo[@]``, ``@foo.*`` ...

  These expand to ``($ foo '*)``, ``($ foo '@)`` etc, thus effectively
  being a special case of dereference via a literal field name.

* ``$foo[expr]``, ``@foo[expr]``

  These expressions expand to correspondingly ``($ foo expr)`` and ``(@ foo expr)``,
  and are useful for accessing array elements.

* ``$foo.xxx[yyy].zzz``

  When dereference clauses are chained, they expand into nested calls to ``$`` and ``@``,
  with the outermost depending on the first character, and all the inner ones being ``@``.

  This example expands to: ``($ (@ (@ foo '$xxx) yyy) '$zzz)``.

* ``@$$foo.bar``, ``$$$foo.bar``

  When the expression contains multiple initial ``$`` characters, all but the first one
  are prepended to the initial variable name.

  These examples expand to ``(@ $$foo '$bar)`` and ``($ $$foo '$bar)``

  **NOTE:** Only the ``$`` character may be used in this way; ``$@@foo.bar`` is invalid.

* ``$.foo``, ``@$[bar]``, ...

  If the expression contains no initial identifier, the initial ``$`` sequence is used
  as one instead (after replacing ``@`` with ``$`` if necessary).

  These examples expand to: ``($ $ '$foo)``, ``(@ $$ bar)``.

  **NOTE:** Unlike the previous syntax pattern, this one uses *all* of the initial
  ``$`` and ``@`` characters.

* ``$(func arg arg...).bar``

  If one initial ``$`` or ``@`` is immediately followed by parentheses, the contents of said
  parentheses are parsed as ordinary lisp code and used instead of the initial variable.

  The example expands to: ``($ (func arg arg...) '$bar)``

* ``@$(foo bar baz)``

  If an initial ``@`` is followed by one or more ``$`` characters and then parentheses,
  it is parsed as a lambda expression (anonymous function) with one argument consisting
  of those ``$`` characters.

  This example expands to: ``(lambda ($) (foo bar baz))``

  **NOTE:** it is an error to use multiple initial ``$`` characters without ``@`` like
  this: ``$$$(...)...``

Basic properties
----------------

As described above, dereference is actually implemented by two generic functions,
``@`` and ``$``, which implement l-value and r-value dereference.

They are defined as such::

    (defgeneric @ (obj key))
    (defgeneric $ (obj key))
    (defgeneric (setf $) (obj key))

Generally, l-value dereference returns an object that can be dereferenced further.
R-value dereference with the same arguments may return the same object as l-value,
or a simple scalar value, depending on the context.

Perhaps oppositely to the used terms, only the r-value dereference function may be
used as the *syntactic* target of assignment; this is because you can't actually change
the (conceptual) address of an object, only its contents; and l-value dereference
returns an address. I.e. in C++ you can write ``*a = ...``, but can't do ``&a = ...``.

Any of the dereference functions may return a list to represent multiple possible
values. Array objects often define ``(@ foo '*)`` to return all of the elements.

If either the obj or key argument of any of the functions is a list (including *NIL*
as empty list), the functions loop over the list, and return a concatenation of the
resulting return value lists. This allows using ``$array.*.field`` to get a list of
all values of a field within array elements.

``($ obj t)`` is defined as the *natural* value of an object; e.g. if obj is a
reference to a numeric field, this will be its value. By default it is equal to
the object itself. ``($ obj key)`` for any other key would fall back to
``($ (@ obj key) t)`` if no special handler for ``$`` with that key and
object was defined.

Reference objects
=================

The ``cl-linux-debug`` library represents typed pointers to objects in memory
as objects of the ``memory-object-ref`` type.

Along with the expected address and type of the pointer, these objects also
retain a history of dereferences that have led to this particular pointer,
and define virtual fields to access this information. This history is similar
to what the Back button in a browser uses.

All references by default have the following properties:

* ``@ref.value``

  By default returns ref itself. May be hidden by struct fields and index-enum keys.

* ``@ref[integer]``

  Returns a reference to address + size*int, i.e. offsets the pointer.

* ``@ref.*``

  Returns a list of contained collection elements. By default empty.

* ``@ref.@``

  Returns a list of subfields. By default empty.

* ``@ref._parent``

  Returns the previous reference in the "back" chain.

* ``@ref._global``

  Returns the nearest reference in the "back" chain that has a globally
  named type, i.e. one defined by a ``struct-type``, ``class-type`` etc,
  and not by any nested substructures. This may return the ref itself.

* ``@ref._upglobal``

  Exactly equivalent to ``@ref._parent._global``.

* ``$ref._address``

  Returns the numeric address embedded in the ref.

* ``$ref._size``

  Returns the size of the object pointed to.

* ``$ref._key``

  Returns the key that was used to get this ref from the parent.
  This is not guaranteed to be precisely accurate, but e.g. for
  array elements this will be the array index.

* ``$ref._type``

  For globally named types, returns their type name.

Primitive types
---------------

Primitive types define the following methods:

* ``$ref[t]``

  The natural value of a primitive field is the scalar non-reference value it contains.

  **NOTE:** When you write ``$struct.field``, it will evaluate via ``($ @struct.field t)``.

* ``@ref.refers-to``, ``@ref.ref-target``

  If the field has the relevant attributes, they can be dereferenced to retrieve the target objects.

Enums
-----

Enum fields return their value as symbols, and allow access to attributes:

* ``$ref[t]``

  Returns the symbol matching the value, unless there is none. May be assigned both as symbol or number.

* ``$ref.attribute``

  If the enum has an attribute with that name, retrieves its value for the current value of the field.

Pointers
--------

* ``$ref[t]``, ``@ref[t]``, ``$ref._target``, ``@ref._target``

  These all return the value of the pointer, i.e. a reference to the target object.

* ``($ ref key)`` -> ``($ (@ ref t) key)``
* ``(@ ref key)`` -> ``(@ (@ ref t) key)``

  All dereferences not explicitly supported are delegated to the target object.
  This means that for most properties pointers are completely transparent; notable
  exceptions are pointers to pointers, and pointers to primitive fields where you
  have to use e.g. ``$struct.ptrfield.value``.

Compounds
---------

* ``@ref.field``, ``@ref._fields.field``

  Returns a reference to the given field.

* ``@ref.*``, ``@ref.@``

  Returns a list of references to all fields. Note that if the object is both an
  implicit compound and a sequence, ``@ref.*`` will returns the sequence items as
  described below.

Sequences
---------

* ``@ref[int]``

  Returns a reference to the Nth item of the sequence.

* ``@ref[symbol]``

  If the sequence has an ``index-enum``, its items can be accessed by symbolic names.

* ``@ref.*``

  Returns a list of all items of the sequence.

* ``@ref._items``

  Returns the items of the sequence as a special lazy object, intended to optimize
  some things in the GUI.

* ``@ref.index-refers-to[int]``

  If the sequence has the relevant attribute, returns the target for the given index.

* ``$ref.count``

  Returns the number of items in the sequence.

* ``$ref.has-items``

  Checks if the sequence has any items, and returns T or NIL.

Code helpers
============

The ``<code-helper>`` tag may be used to add lisp code fragments
to the objects defined in the xml. The ``refers-to``, ``index-refers-to``
and ``ref-target`` tags are also converted to code helpers internally,
and you can use e.g. ``<code-helper name='refers-to'>...</code-helper>``
instead of the attribute if your expression is too long for it.

There are two features that can only be implemented via explicit
``<code-helper>`` tags:

* ``<code-helper name='describe'> ... </code-helper>``

  This specifies a piece of code that is called to supply additional
  informational items for the rightmost column of the table in the GUI
  tool. The code should return a string, or a list of strings.

  As with ``refers-to``, the code receives the value of the object
  as ``$``, and the reference to the object in ``$$`` (i.e. ``$`` is
  equal to ``$$[t]``).

  The ``(describe-obj object)`` function can be used to call the same
  describe mechanism on another object, e.g.::

    <code-helper name='describe'> (describe-obj $.name) </code-helper>

* ``<code-helper name='find-instance'> ... </code-helper>``

  If the ``instance-vector`` and ``key-field`` attributes are not descriptive
  enough to specify how to find an instance of the object by id, you can explicitly
  define this helper to be used by ``ref-target`` links elsewhere.

  It receives the value of the ``ref-target`` bearing field as ``$``,
  and its ``aux-value`` as ``$$``.

  Other than via ``ref-target``, you can invoke this mechanism explicitly using
  the ``(find-instance class key aux-key)`` function, even from a ``find-instance``
  helper for another type::

    <code-helper name='find-instance'>$(find-instance $art_image_chunk $$).images[$]</code-helper>

  This finds an instance of the ``art_image_chunk`` type using the aux-value ``$$``,
  and then returns an element of its ``images`` sub-array using the main value ``$``.

Examples
========

* ``@global.*``

  The global variable 'global' contains a special compound that contains
  all known global objects. This expressions retrieves a list of refs to
  all of them.

  Using ``$global.*`` would return values for the primitive ones instead
  of refs, and is not that useful.

* ``$global.world.units.all[0].id``

  This expression is syntactically parsed into the following sequence::

    tmp = global
    tmp = @tmp.world  ; the world global ref
    tmp = @tmp.units  ; the units field ref
    tmp = @tmp.all    ; the all vector ref
    tmp = @tmp[0]     ; the first unit object pointer ref
    $tmp.id

  The only non-trivial step here is the last one. The last value of
  tmp is a reference to a pointer, and as described above, it delegates
  anything it does not directly understand to its target, adding an
  implicit step at runtime::

    unit = @tmp._target
    $unit.id

  A unit object does not define ``$unit.id`` directly either, so the
  final step falls back to::

    idref = @unit.id
    ($ idref t)

  which retrieves a reference to the ``id`` field, and then evaluates
  its natural value.

  The result is that the expression returns the id value of the first
  unit in the vector as would be naturally expected.

  Using ``@global.world.units.all[0].id`` would have used ``@tmp.id`` as
  the last step, which would have skipped the ``($ idref t)`` call and
  returned a reference to the field.

* A simple ``index-refers-to`` example::

    <stl-vector name='created_weapons' type-name='int32_t'
                index-refers-to='$global.world.raws.itemdefs.weapons[$]'/>

  This is used to define a vector with counts of created weapons.

  When it is displayed in the GUI, the tool evaluates the ``index-refers-to``
  expression for every vector element, giving it the *element index*
  as ``$``, and a reference to the vector itself as ``$$`` (here unused).

  The expression straightforwardly uses that index to access another
  global vector and return one of its elements. It is then used by the
  GUI to add additional information to the info column.

* An example of ``refers-to`` and ``_parent``::

    <compound name='burrows'>
        <stl-vector name='list' pointer-type='burrow'/>
        <int32_t name='sel_index' refers-to='$$._parent.list[$]'/>
    </compound>

  This fragment of XML defines a compound with two fields, a vector and an int,
  which has a ``refers-to`` attribute. When that field is displayed in the GUI,
  it evaluates the expression in the attribute, giving it the *integer value*
  as ``$``, and a *reference* to the integer field as ``$$``.

  The expression parses as::

    tmp = $$            ; reference to the int32_t field
    tmp = @tmp._parent
    tmp = @tmp.list
    $tmp[$]

  Since the only way the GUI could get a reference to the field was to evaluate
  ``@ref-to-burrows.sel_index``, that previous reference is stored in its "back"
  list, and ``@tmp._parent`` retrieves it. After that everything is simple.

* An example of ``ref-target`` with ``aux-value``::

    <int32_t name='race' ref-target='creature_raw'/>
    <int16_t name='caste' ref-target='caste_raw' aux-value='$$.race'/>

  The ``race`` field just specifies a type as ``ref-target``, so the
  reference simply evaluates the ``find-instance`` helper of the
  ``creature_raw``, passing it the race value as ``$``.

  In order to find the caste however, you need to first find a creature,
  which requires a race value. This value is supplied via the ``aux-value``
  attribute into the ``$$`` argument to ``find-instance``.

  Since the value of the ``caste`` field will be passed through to the
  helper anyway, when evaluating ``aux-value`` the ``$`` argument is set
  to a *reference* to the holding field, and ``$$`` is set to its ``_parent``.
  This means that ``$$.race`` in the context of ``aux-value`` is equivalent
  to ``$$._parent.race`` in the context of ``refers-to``.

* A complex example of cross-references between arrays::

    <struct-type type-name='caste_raw'>
        <compound name='body_info'>
            <stl-vector name='body_parts' pointer-type='body_part_raw'/>
        </compound>
        <compound name='bp_appearance'>
            <stl-vector name='modifiers' pointer-type='bp_appearance_modifier'/>

            <stl-vector name='modifier_idx' type-name='int32_t'
                        refers-to='$$._parent._parent.modifiers[$]'
                        index-refers-to='$$._parent.part_idx[$].refers-to'/>
            <stl-vector name='part_idx' type-name='int16_t'
                        refers-to='$$._global.body_info.body_parts[$]'/>
            <stl-vector name='layer_idx' type-name='int16_t'
                        refers-to='$$._parent._parent.part_idx[$$._key].refers-to.layers[$]'
                        index-refers-to='$$._parent.part_idx[$].refers-to'/>
        </compound>
    </struct-type>

  In order to understand this example it is first necessary to understand
  that ``refers-to`` specified on a vector is actually transplanted onto the
  implicitly constructed element tag::

    <stl-vector name='part_idx'>
        <int16_t refers-to='$$._global.body_info.body_parts[$]'/>
    </stl-vector>

  Therefore, ``$$`` is a reference to the ``<int16_t>`` field,
  ``$$._parent`` is a reference to the vector, ``$$._parent._parent``
  is a reference to the ``bp_appearance`` compound, etc.

  The ``$$._global...`` works as an abbreviation that applies ``_parent``
  until it reaches a globally defined type, which in this case is the
  current instance of the ``caste_raw`` struct.

  **NOTE:** ``$$._global._global`` is the same as ``$$._global``, i.e.
  repeated ``_global`` is a no-op. The latest version supports ``_upglobal``,
  which is equivalent to ``_parent._global``.

  Thus, the ``refers-to`` link on the ``part_idx`` vector evaluates to
  the element of the ``body_parts`` vector, indexed by the *value* of the
  current ``part_idx`` vector item.

  Likewise, the ``refers-to`` link on the ``modifier_idx`` vector goes
  back to the ``bp_appearance`` compound, and descends into the ``modifiers``
  vector, using the value of the current item.

  The ``index-refers-to`` link on the same ``modifier_idx`` vector
  highlights the shared indexing relation between the bottom vectors
  by linking to the part_idx vector via the current item *index*.
  Since this attribute is hosted by the vector itself, ``$$`` points
  at the vector, and only one ``_parent`` is needed to reach
  ``bp_appearance``.

  This link also demonstrates how the defined relations can be reused
  in other expressions by accessing the target of the ``refers-to``
  link inside ``part_idx``. When the ``part_idx`` vector is accessed
  simply as ``$xxx.part_idx[foo]``, it evaluates as::

    tmp = @xxx.part_idx
    tmp = @tmp[foo]
    ($ tmp t)

  thus returning just an integer value. However, if an additional
  dereference step is added, it turns to::

    tmp = @xxx.part_idx
    tmp = @tmp[foo]
    obj = @tmp.refers-to
    ($ obj t)

  which follows the ``refers-to`` link and evaluates its target.

  Finally, the ``layer_idx`` vector, in addition to specifying the same
  ``index-refers-to`` link as ``modifier_idx``, uses the link in ``part_idx``
  to access other objects at its end::

    refers-to='$$._parent._parent.part_idx[$$._key].refers-to.layers[$]'

  Note how this link has to use two ``_parent`` steps again due to being
  attached to the element of the vector instead of the vector itself.
  It also has to use the ``_key`` attribute of the vector element to
  retrieve the current index in the vector, because here ``$`` holds the
  element value.
