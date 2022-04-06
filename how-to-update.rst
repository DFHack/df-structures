###########################################
Updating DF-structures for a new DF version
###########################################

.. contents:: Contents
  :local:
  :depth: 1

General Process
===============
Download the new versions. The scripts expect the following
directory layout::

    ~userhome/
        Games/
            DF/
                df_linux/    - Current DF for linux
                df_windows/  - Current DF for windows
                df_osx/      - Current DF for osx

                metasm/      - Checkout of the library for ruby scripts
                               from https://github.com/jjyg/metasm
                df_misc/     - Checkout of the ruby scripts
                               from https://github.com/jjyg/df_misc
                dfhack/      - DFHack checkout

- Use "new-release.pl v0.??.??" to automatically perform a number of
  steps. If you get a mismatch error from match-ctors.pl, see "STAGE 1".
- Start the linux DF version, and launch the tool.
- Execute (reset-state-annotation)
- Commit.
- Use the tool to verify that the layout of the compound globals
  on linux is correct, and update xml as necessary. Check that
  unit seems reasonable, etc. Compare the changes in g_src between
  releases. Delete redundant entries for some linux & osx globals
  in symbols.xml.
- Compile DFHack without plugins for windows and run devel/find-offsets
  to find globals there.
- With the windows version in wine, run (check-struct-sizes) to see if
  any objects changed their size. If nothing is obviously wrong,
  use (check-struct-sizes :annotate? t) to mark correctly sized
  objects ALIGNED.
- Check the rest of the document for info on finding still missing globals.
- Commit.
- Run make-csv.sh to update CSV files and verify vtable size and argument counts.
- Commit.


Running Dwarf Fortress
======================

.. highlight:: shell

The lisp tool expects that the game is started in a mode where all
allocated memory is automatically filled with a certain byte value.

On linux this is achieved by simply starting the game like this::

    MALLOC_PERTURB_=45 ./df

Windows requires applying a patch to a copy of the executable like this::

    cp -f 'Dwarf Fortress.exe' 'Dwarf_Fortress_malloc.exe'
    ruby -I ~/Games/DF/metasm ~/Games/DF/df_patchmalloc.rb 'Dwarf_Fortress_malloc.exe'


Available Scripts
=================

new-release.pl
--------------
Takes the full v0.??.?? version number as one required parameter.

Uses md5sum for linux and winedump for windows to find
out the new hash and PE timestamp.

Use the stamps to create new sections in symbols.xml
Also paste them into make-csv inside start.lisp

Creates an empty v0.??.??.lst, and change thes open-annotations
filename in start.lisp.

Wipes linux/df.globals.xml empty of all the global definitions.

Runs make-scans.sh to find out addresses of vtables and
many linux/osx globals, and pastes them into symbols.xml

make-scans.sh
-------------
Runs ruby and perl scripts to extract data from the executables,
and writes the output to txt files in subdirectories.

make-csv.sh
-----------
Uses the lisp tool and some scripts to produce csv files with
offsets for linux and windows. These are useful for manual lookup
and some scripts.

start.sh
--------
Starts the lisp tool. You may pass the process ID as a parameter
to avoid the prompt.

make-keybindings.pl
-------------------
Used by make-scans to extract the keybinding enum from g_src
in form of df.keybindings.xml

match-ctors.pl
--------------
Used by make-scans to compare the extracted addresses of the
compound linux/osx globals with a saved copy from a previous
version and thus determine their names.

match-vtables.pl
----------------
Used by make-csv.sh to produce a file listing the addresses of
all virtual methods in a compact form. Relies on csv files and
data from make-scans.sh


STAGE 1. Linux compound globals
===============================
(done by new-release.pl normally)

Linux and OSX initalize and destruct their complex globals in
a way that allows to determine their addresses by disassembling
a small section of the executable. This is currently done by
ruby scripts called from new-release.pl; it is also possible to do
that via the lisp tool for linux.

The ruby scripts produce a raw dump of the global addresses as
linux/ctors.txt. A perl script is then used to compare it with
linux/ctors-base.txt (which is manually edited and committed into the
repository), and thus derive the names of the globals by their
order. The resulting data is written back to linux/ctors.txt,
linux/df.globals.xml and linux/cglobals.txt (which is inserted
into symbols.xml).

If the size of a global changes too much, or a new one is added
in the middle, this matching may fail. In this case it is necessary
to manually match and add the new names to ctors.txt and commit
it as ctors-base.txt. After that, run make-scans.sh to rerun
the scripts, and paste linux/cglobals.txt into symbols.xml.

OSX behaves exactly the same as linux in this respect.


STAGE 2. Old way to find Linux compound globals
===============================================
(now mostly obsolete, retained as fallback and for historical interest)

Globals gps, enabler, gview and init are in the export table
for linking with libgraphics, so they are immediately available
in (browse @global.*).

Run (list-globals/linux), paste the results in linux/df.globals.xml,
and immediately compare it to the old version from source control.
The order of the globals is quite stable, so if sizes look similar,
they can be guessed immediately.

The .bss compound section should be done except for 'announcements'.

Run (browse-dataseg). The first three -30000 are cursor. Following
group of 6 are selection_rect. After that, at 16-aligned addresses
are control_mode and game_mode. Tab the game ui to the most common
two-pane mode, scroll to the end and find 0x30200. Within this dword
ui_menu_width is byte 1, ui_area_map_width is byte 2.

(reload), (browse @global.*), look at the most important globals
for misalignment. If found, fix it and delete old tables from
symbols.xml.


STAGE 3. Linux primitive globals
================================
Unpause the game for a moment to let various structures be initialized.

The fields can be found either by a straight memory search, or by
looking in the area they are expected to be.

[A] The 'cur_year' area.
------------------------
Located just before ui_building_assign_type.

1. cur_year / cur_year_tick

   (find-changes); step with dot; Enter; step; +; step; +; step; +; done

   look at values in bss, there will be cur_year_tick, and
   cur_year is 32 bytes before that.

2. process_jobs

   Designate a building for construction.
   Look after process_dig for an enabled boolean.

3. process_dig

   Step the game one step. Designate a tile for digging.
   Look after cur_year and before process_jobs.

   Note: this order because designating sometimes sets process_jobs too.

4. job_next_id / ui_workshop_job_cursor

   Find a workshop without jobs; (find-changes); add job; Enter;
   add job; +; add job; +; done
   Finds job_next_id and ui_workshop_job_cursor, the distinction is obvious.

   The ui_workshop_job_cursor is expected to be after cur_year_tick.

5. ui_workshop_in_add, ui_building_in_resize, ui_building_in_assign

   Expected to be in the area after ui_workshop_job_cursor, in this order.
   Change the relevant state in game and F5.

6. ui_building_item_cursor

   Find a cluttered workshop, t; (find-changes); move cursor down; Enter;
   cursor down; +; cursor down; +; done

   Expected to be right after ui_workshop_job_cursor.

7. current_weather

   Subtract 0x1c from cur_year address. Obviously, a big hack.

   It is best to use a save where the contents are non-zero and known to you.

[B] The ui_look_cursor area.
----------------------------
Located in the area of the 124 byte global before ui.

1. ui_look_cursor

   Like ui_building_item_cursor, but with a cluttered tile and k.

2. ui_selected_unit

   Find a place with many nearby units; (find-changes); v; Enter; v; new;
   ...; when returned to origin, 0; 1; 2...; done

   Expected to be before ui_look_cursor.

3. ui_unit_view_mode

   Select unit, page Gen; (find-changes); Inv; Enter; Prf; +; Wnd; +; done

   Expected to be after ui_selected_unit.

4. pause_state

   (find-changes); toggle pause; Enter; toggle; 0; toggle; 1; etc; done

   Expected to be in the area after ui_look_cursor.

[C] The window_x/y/z area.
--------------------------
Located right after ui_build_selector.

1. window_x, window_y, window_z

   Use k, move window view to upper left corner, then the cursor to bottom
   right as far as it can go without moving the view.

   (find-changes); Shift-RightDown; Enter; Shift-RightDown; + 10;
   Shift-RightDown; + 10; done

   Finds cursor and two variables in bss. Z is just after them.

[D] Random positions.
---------------------
1. announcements

   Immediately follows d_init; starts 25 25 31 31 24 ...


STAGE 4. Primary windows compound globals
=========================================
After aligning globals on linux, run (make-csv) to produce offset tables.

1. world
--------
Set a nickname, search for it; the unit will have it at offset 0x1C.
Then trace back to the unit vector, and subtract its offset.

2. ui
-----
Open the 's'quad sidebar page. Navigate to a squad in world.squads.all,
then backtrace and subtract the offset of ui.squads.list.

3. ui_build_selector
--------------------
Start creating a building, up to the point of material selection.
Find the material item through world and backtrack references until .bss.

4. ui_sidebar_menus
-------------------
Select a unit in 'v', open inventory page, backtrack from
unit_inventory_item, subtract offset of unit.inv_items.

5. ui_look_list
---------------
Put a 'k' cursor over a unit, backtrack to a 0x10 bytes object
with pointer at offset 0xC, then to the global vector.

6. ui_advmode
-------------
In adventure mode, open the 'c'ompanions menu, then backtrack from
world.units.active[0] (i.e. the player) via ui_advmode.companions.unit

Alternatively, look before ui_look_list for "0, 15" coming from the string.

7. enabler
----------
(find-changes), resize the window, enter; resize width by +1 char,
+; repeat until few candidates left; then done, select the renderer
heap object and backtrack to enabler.renderer.

Alternatively, look before ui for clocks changing every frame.

8. map_renderer
---------------
Put a 'v' cursor exactly above a unit; backtrack from the unit object.

Alternatively, look before ui_advmode for the unit pointer list.

9. texture
----------
Load the game with [GRAPHICS:YES] in init.txt, and example set.
Then search for string "example/dwarves.bmp" and backtrack.

Alternatively, look between ui_build_selector and init.


STAGE 5. Secondary windows compound globals
===========================================
These are too difficult to find by backtracking or search, so try
looking in the expected area first:

1. timed_events
---------------
Look for a pointer vector around -0x54 before ui.

2. ui_building_assign_*
-----------------------
2a. ui_building_assign_is_marked

    Assign to zone, (find-changes), toggle 1st unit, enter; toggle 1st,
    0; toggle 1st, 1; toggle 2nd, new; done

    The vector is expected to be just before ui.

2b. ui_building_assign_items

    Expected to be immediately before ui_building_assign_is_marked.

2c. ui_building_assign_units

    Start assigning units to a pasture, backtrack from one of the units.

    The vector is expected to be immediately before world.

2d. ui_building_assign_type

    The vector is expected to be 2nd vector immediately after ui_look_list.

3. gview
--------
Immediately follows ui.

4. Init files
-------------
4a. d_init

    Follows world after a small gap (starts with flagarray).

4b. init

    Follows ui_build_selector after a small gap.

5. gps
------
Look at around offset ui_area_map_width+0x470 for pointers.

6. created_item_*
-----------------
6a. created_item_type

    Expected to be at around -0x40 before world.

6b. created_item_subtype

    The first vector immediately after ui_look_list.

6c. created_item_mattype

    Immediately before ui_sidebar_menus.

6d. created_item_matindex

    Before ui, after timed_events.

6e. created_item_count

    Immediately before timed_events.


STAGE 6. Windows primitive globals
==================================
Like linux primitives, except the ordering is completely different.

This section only describes the ordering heuristics; for memory search
instructions see linux primitive globals.

[A] formation_next_id

    Followed by ui_building_item_cursor, cur_year.

[B] interaction_instance_next_id...hist_figure_next_id

    Contains window_x, ui_workshop_in_add.

[C] machine_next_id

    Followed by ui_look_cursor, window_y.

[D] crime_next_id

    Followed by, in this order (but with some gaps):

    - ui_workshop_job_cursor
    - current_weather (immediately after ui_workshop_job_cursor)
    - process_dig
    - process_jobs
    - ui_building_in_resize
    - ui_building_in_assign
    - pause_state

[E] Random positions.

  1. cur_year_tick

    Look immediately before artifact_next_id.

  2. window_z

    Look before proj_next_id.

  3. ui_selected_unit

    Look just after squad_next_id.

  4. ui_unit_view_mode

    Look just before hist_event_collection_next_id.

  5. announcements

    Immediately follows d_init; starts 25 25 31 31 24 ...
