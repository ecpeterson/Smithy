Smithy - a cross-platform Forge clone

Overview
========
    Forge, the original map editor for the Marathon series of games, was released to the public in 1997 as a Mac OS 7 application.  While dated and buggy, it's become the de facto standard map editor for the Marathon mapping community, largely because it was crafted by the original creators of Marathon, who knew exactly what they wanted to see in a map editor.  The handful of serious attempts at a more modern map editor were by and large attempted by programmers who had no real concept of what made a map editor useful to the general mapping public.  Sadly, Forge is a doomed application, first because Apple is switching to an x86 architecture (which prevents classic applications such as Forge from being run) and second because Mac OS 10.5 isn't capable of running classic applications even on PowerPC machines.  Smithy's goal is to be a successful Forge replacement by exactly replacing Forge, replicating it in every way -- at least as a starting point.  It must also be cross-platform and upgradeable to prevent it from falling into the same trap as Forge.  OCaml, a fast, cross-platform, functional language, fit the bill reasonably well, and coupled with the LablGTK and LablGL bindings, the project has been very manageable.

License
=======
    Smithy is released under the University of Illinois / NCSA license.  See docs/license.txt for more information.

Usage
=====
    Upon launch, Smithy opens a map view window and a toolbox window, and if it was passed a filename on the command line it opens it.  Maps can also be opened using the File -> Open menu item.  From here, the tools in the toolbox can be applied to the map to alter the viewpoint of the window and the geometry data contained therein, while the menus at the top of the map view window contain tools that either modify global map settings or alter the view mode of the map view window.  When edits are finished, the modified map can be committed to disk by the Save command in the File menu.

Tools
=====
 + Arrow tool: the arrow tool is used to select, move, and inspect objects.  A map element can be selected by left-clicking near it.  A map element that is selected can be moved by dragging the mouse with the left mouse button held down.  A map element can be inspected by right-clicking near it, which will open an inspection dialog, where the settings for the element in question can be altered.
 + Line tool: the line tool is used to add points and lines to the map geometry.  A single left click will add a point to the map.  A left mouse button drag will add a line to the map, along with two points at the line segment's endpoints.  All of the above-mentioned points will be automatically joined to nearby points and will split lines that are not attached to preexisting polygons.
 + Poly tool: the polygon tool draws rectangular polygons onto a map.  (not currently implemented)
 + Fill tool: the fill tool attaches a polygon to a line loop upon a left-click.
 + Pan tool: the pan tool scrolls the map surface around the map view window.
 + Zoom tool: the zoom tool zooms in on the position under the mouse on a left-click and zooms out over the position under the mouse on a right-click.
 + Text tool: the text tool adds a map annotation. (not currently implemented)
 + Object tool: the object tool adds an object to the map.

View Modes
==========
    Separate from the Draw mode, there are several modes that visualize various attributes of polygons, including height, texture, lighting, media, and sound.  The particular attribute associated with a polygon can be entered manually in the secondary toolbox that appears when entering an alternative mode, or can be picked from preexisting polygons by right-clicking. Attributes can then be assigned to a polygon by left-clicking.

Features
========
 + Load and save unmerged map files
 + Edit point / line / polygon geometry
 + Display, move, alter, add, and remove objects
 + Assign polygon types and attributes (lights, elevations, liquids)
 + Alter light and liquid attributes
