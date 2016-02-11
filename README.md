# Crawler

Crawler is an experimental dungeon generation library. The goal is to be able to procedurally generate a dungeon for use in a rogue-like, or really any dungeon-exploration game.

## Installation

Crawler is not included in QuickLisp yet. If you would still like to try it out, you can simply clone this repository to your QuickLisp local-projects directory.

## Usage

If you just want to generate a dungeon without rendering it:

```lisp
(ql:quickload :crawler)
(crawler:make-dungeon 99 49)
```

All of the data you need to render a dungeon is located in the array (tile-map *dungeon*).

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

* White: Walkable floor tiles
* Dark gray: Un-walkable wall tiles
* Blue: Junctions between two regions. A game can use these to make doors, etc.
* Red: The start of the dungeon. A game can use this to place a staircase leading up, etc.
* Green: The end of the dungeon. A game can use this to place a staircase leading down, etc.

## Input

Within the example window the following input is accepted:

* Left mouse-click: Generate and display a new dungeon of the same size.

## Examples

![Example Dungeon](/images/example.png)

## TODO
* Ability to create different types of maps, such as natural cave-like systems.

If you have another suggestion, just let me know, and I'll see if it makes sense to implement.
