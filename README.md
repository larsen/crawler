# Crawler

Crawler is an experimental dungeon generation library. The goal is to be able to procedurally generate a dungeon for use in a rogue-like, or really any dungeon-exploration game.

## Installation

Crawler is not included in QuickLisp yet. If you would still like to try it out, you can simply clone this repository to your QuickLisp local-projects directory.

## Usage

If you want to try out the example graphical dungeon generator, load the example system and generate a dungeon with:

```lisp
(ql:quickload :crawler-examples)
(crawler-examples:random-dungeon 99 49 10)
```

This will create a dungeon that is 99x49 tiles, with a tile size of 10x10, just as the example images below. The example uses [Sketch](http://github.com/vydd/sketch) to display the dungeon graphically. Note that this requires having SDL2 installed on your operating system.

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

* ~~Make corridors be less windy.~~
* ~~Enforce junctions adjacent to each other.~~
* Many more fixes.

If you have another suggestion, just let me know, and I'll see if it makes sense to implement.
