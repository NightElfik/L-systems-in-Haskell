
Wandering
=========

Wandering is generic maze generator, solver, and game playable in a console.
It supports hexagonal and rectangular mazes and two types of maze generators
Cool thing is that the maze type is independent of the solver.
Every solver can solve every type of maze.
It is possible to define new maze types, generators, and solvers.
Another cool feature is the UI system in a console including menus, text boxes, windows, and screen manager.
This was my first larger program written in C++.

**Project page**: http://www.marekfiser.com/Projects/Wandering

**Disclaimer**: This project was my first project written in C++ in 2011.
Some techniques used in this proejct are not very smooth (such as including C++ files using `#include` :).
I am not planning to fix it or work on it anymore.
I am sharing this project mostly because of its cool features.

**License**: Public domain, see LICENSE.txt for details.

Features
--------

* Generic maze generator being able to generate rectangular and hexagonal mazes. It can be extended to support even more types.
  * There are two types of maze geration algorithms - recursive back-tracker and prism algorithm. Both are written in a was that they can handle generic mazes.
* Generic maze solver with similar properties as generator.
* Retro console UI with screen manager, wondows, shadows, text boxes and buttons. All written in somewhat generic way.
* In-game menu system.
* Single player and split-screen multi-player modes.
* Hint functionality.
* Easter eggs (since it was a school project :).



Example maze
------------
```
   ____        ____        ____        ____
  /    \      /    \      /    \      /    \
 /   ☺  \____/      \____/      \____/      \____
 \      /    \                                   \
  \    /      \____        ____        ____       \
  /    \           \           \      /           /
 /      \           \____       \____/           /
 \      /    \      /           /           /    \
  \    /      \    /       ____/       ____/      \
  /           /    \           \           \      /
 /       ____/      \____       \____       \    /
 \      /    \           \           \      /    \
  \____/      \____       \____       \    /      \
  /    \           \           \      /    \      /
 /      \           \____       \____/      \    /
 \      /    \           \                  /    \
  \    /      \____       \____        ____/      \
  /    \           \      /    \      /           /
 /      \____       \    /      \____/       ____/
 \           \      /           /    \      /    \
  \           \    /           /      \    /      \
  /    \      /    \      /           /    \      /
 /      \____/      \____/           /      \    /
 \      /           /    \      /    \           \
  \    /       ____/      \____/      \____       \
  /    \      /                \           \      /
 /      \    /                  \____       \    /
 \           \      /    \           \           \
  \           \____/      \____       \____       \
  /    \      /           /    \           \      /
 /      \____/       ____/      \____       \____/
 \                  /                            \
  \____        ____/       ____        ____   ♥♥  \
       \      /    \      /    \      /    \  ♥♥  /
        \____/      \____/      \____/      \____/
```