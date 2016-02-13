# Crawler

Crawler is an experimental dungeon generation library. The goal is to be able to procedurally generate a dungeon for use in a rogue-like, or really any dungeon-exploration game.

## Installation

Crawler is not included in QuickLisp yet. If you would still like to try it out, you can simply clone this repository to your QuickLisp local-projects directory.

## Usage

If you would like to try out the library visually, the provided examples may be of use. Jump to the Examples section below.

If you just want to generate the dungeon data without rendering it:

```lisp
(ql:quickload :crawler)
(crawler:make-dungeon 99 49)
```
`CRAWLER:MAKE-DUNGEON` takes 2 required parameters, width and height, and a number of optional parameters.

The optional parameters are specified as keyword/values:

* `:WINDINESS`: Specifies how windy corridors will be, as a range from `0.0` to `1.0`. This has minimal effect for densely packed rooms - see `:ROOM-DENSITY` below to control this. Default value is `0.0`.
* `:ROOM-DENSITY`: Specifies how densely rooms should be packed in the dungeon, as a range from `0.1` (10%) to `1.0` (100%). Note: This is only an estimation. Default value is `0.65`.
* `:ROOM-SIZE-MIN`: Specifies the minimum size in tiles the width or height of a room is allowed to be, within the range `3` to `99`. Note: This should be supplied as an odd integer value, else it will be decremented by 1. Default value is `3`.
* `:ROOM-SIZE-MAX`: Specifies the maximum size in tiles the width or height of a room is allowed to be, within the range `ROOM-SIZE-MIN` to `101`. Note: This should be supplied as an odd integer value, else it will be decremented by 1. Default value is `11`.
* `:JUNCTION-RATE`: Specifies the percentage of extra doors to generate between 2 regions, as a range from `0.0` to `1.0`. A value of `1.0` will place a junction in every possible tile location, causing many loops in the generated dungeon. Note: A value of `0.0` only affects extra junctions, and will therefor allow only 1 junction connecting a pair of regions. Default value is 0.03.

All of the data you need to render a dungeon is located in the array `(tile-map *dungeon*)`.

This is an array of `Tile` instances, each having the following slots:

* `X`: The X location in the map.
* `Y`: The Y location in the map.
* `WALKABLEP`: Predicate determining whether this tile is ground that can be walked upon.
* `REGION-ID`: An integer representing the region. A region is a group of adjacent tiles. All rooms are of the same `REGION-ID`. Likewise, a corridor between two rooms (or more, in the case of branching) is of the same `REGION-ID`. You can think of a `REGION-ID` as belonging to a set of tiles as if it was flood-filled, stopping at junctions (what crawler calls doors or whatever your game may define them as).
* `ADJACENT-REGIONS`: A list of `REGION-ID`s specifying all the unique region's orthogonally adjacent to this tile.
* `MAP-FEATURES`: A list of symbols identifying a special tile property, if any. Currently this can be one or more of the following:

** `:JUNCTION`: The tile joins 2 unique regions.
** `:STAIRS-UP`: The tile with an entrance staircase.
** `:STAIRS-DOWN`: The tile with an exit staircase.
** `:ROOM`: The tile is in a room.
** `:CORRIDOR`: The tile is a corridor.

## Examples

![Example Dungeon](/images/example.png)

If you want to try out the example graphical dungeon generator graphically, load the example system and generate a dungeon with:

```lisp
(ql:quickload :crawler-examples)
(crawler-examples:random-dungeon 99 49)
```

Note: The examples require the following packages which are NOT part of Quicklisp. You must manually clone and place these libraries in Quicklisp's local-projects directory:
* [Sketch](http://github.com/vydd/sketch)
* [cl-sdl2-image](http://github.com/lispgames/cl-sdl2-image)
* [cl-sdl2-ttf](http://github.com/Failproofshark/cl-sdl2-ttf)

Additionally, Sketch requires that you have SDL2 installed with your OS.

The examples use the following colors for different tiles of the dungeon:

* White: Walkable floor tiles.
* Dark gray: Un-walkable wall tiles.
* Blue: Junctions between two regions. A game can use these to make doors, etc.
* Red: The start of the dungeon. A game can use this to place a staircase leading up, etc.
* Green: The end of the dungeon. A game can use this to place a staircase leading down, etc.

Within the example window the following input is accepted:

* Left mouse-click: Generate and display a new dungeon of the same size.

## TODO
* Ability to create different types of maps, such as natural cave-like systems.

If you have another suggestion, just let me know, and I'll see if it makes sense to implement.
