# Crawler

Crawler is an experimental dungeon generation library. The goal is to be able to procedurally generate a dungeon for use in a rogue-like, or really any dungeon-exploration game.

In its current form, Crawler is not very usable, as it is in its infancy and is lacking features and fairly incomplete.

## Installation

As mentioned, Crawler is incomplete and is not included in QuickLisp yet. If you would still like to try it out, you can simply clone this repository to your QuickLisp local-projects directory.

## Usage

If you want to try out the example graphical dungeon generator, load the example system and generate a dungeon with:

```lisp
(ql:quickload :crawler-examples)
(crawler-examples:random-dungeon :w 99 :h 49 :tile-size 10)
```

This will create a dungeon that is 99x49 tiles, with a tile size of 10x10, just as the example images below. The example uses [Sketch](http://github.com/vydd/sketch) to display the dungeon graphically. Note that this requires having SDL2 installed on your operating system. Also note that the dungeon algorithm looks best with odd values for the dungeon width and height.

## Input

Within the example window the following input is accepted:

* Left mouse-click: Generate and display a new dungeon of the same size.
* Right mouse-click: Toggle between terrain and region modes.

Terrain Mode displays each 'terrain' in a different color. There are three types of terrain:

* Black: Walls
* White: Corridors
* Blue: Rooms
* Red: Doors

Region Mode displays each unconnected area in a different color. This means each room is a different color, and possibly different parts of the winding corridors. Also, possible locations for a door are marked with a red circle. These are walls that are connecting 2 different regions.

## Examples

### Terrain Mode Example

![Terrain Example](/images/example-terrain.png)

### Region Mode Example

![Region Example](/images/example-regions.png)

## TODO

As mentioned, this project is very incomplete. The following is a list of features to add:

* Make corridors be less windy.
* ~~Enforce extra doors to not be adjacent to existing doors.~~
* Many more fixes.

If you have another suggestion, just let me know, and I'll see if it makes sense to implement.
