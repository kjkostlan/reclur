# Reclur

**Reclur** is a Clojure editor with a highly custom UI. It has some of the standard features one would expect in an IDE along with these more unique aspects:

## The most of space

Since when did GUI's have to be so *clumsy*?

**Large virtual desktop:** Set up many workspaces and pan/zoom your view between them. Move, copy and paste windows or even whole workspaces at once. Combine or release windows from tabs at-will.

**No wasted real-estate:** Scroll bars take up two pixels. Line numbers are squeezed in and take little space.
The title bar is minimal. Windows are slightly transparent which allows getting a sense of what is underneath while still showing the top one clearly.

**Multiple views:** A single file can have multiple views open, editing one edits all in sync, code-folded or not.

## The depths of Clojure

Reclur is a very pure Clojure editor with little Java interop.

**Hand-rolled GUI:** The entire GUI is vanilla Clojure drawn on a single JPanel. The only interaction with Java widgets is receiving events, file I/O, and rendering graphics.

**Persistent Data Structures:** Mutation is only at the top level and bottom levels. At the bottom there is high-performance functions that use arrays internally. At the top there is a global app-state and external entities (files, logging, reloading namespaces). Everything else lives in the world of persistent data structures.

**Leave Java first, enter last:** Java events such as awt.event.mousePressed are immediately converted into Clojure maps. Similarly, the graphics are stored as Clojure data structures up until the very last moment where they get rendered to java's Graphics2D.

## Introspection

Self-awareness is vital... for both human being or and computer programs!

**Logging system:** Put loggers almost anywhere inside of functions to see what is going on. It's easy to add several logger-REPLs, or add a logger-REPL that always points to the last codebox cursor-location you clicked.

**Browse large REPL reports:** Have a complicated nested map? Click on the REPL's output to zoom in. Complete with a "smart" pretty printer. Handles infinite data structures.

**Auto-update:** Automatically updates namespaces when saved.

## Setup guide

1. Install clojure and [leiningen](https://leiningen.org/).

2. Run the reclur.bat file (windows) or the reclur.sh (mac and linux). It will download a few dependencies.

3. Save your project in src/client/yourProject. Anything in client is .gitignored. Clojure thinks it is all one project, however, so namespaces must be qualified to be in client.yourProject.yourCljFile, and dependencies must be added to the global.lein file. You can add src/startup.clj file which runs on launch and will also be .gitignored.

## Development status
This project is **Alpha/Beta** and development is *on hold* since I have migrated to Python. Major bugs should be worked out (it won't hurt to backup your data just in case) and a few "standard" features are missing.

**I plan on recapturing the beauty of Clojure and the features of Reclur in my newer TuringCreate project**

## Other interesting Clojure editors

All of these have been active as of 2021 and some are still active to this day.

**LightTable/LightTable**: Enables *very* real-time coding. Also handles javascript and has an extensive plugin system. This branch is the development branch.

**mogenslund/liquid**: Another attempt to make a pure clojure editor which minimizes mutation. Based out of the terminal.

oakes/Paravim: A vim clojure plugin.

clojure-emacs/cider-nREPL: An emacs must-have for clojure programmers. Very active.

eerohele/Tutkain: A sublime text clojure plugin written in Python. Very active.

PEZ/rich4clojure: A teaching clojure tool from your browser.

Cirru/calcit-editor: Edit clj files like trees instead of as a text file. Trees are stylized with CSS and the parentheses are hidden. The code defines the behavior of various "comps".

arthuredelstein/clooj: The repo itself has been dead since 2014. However, clooj was used as a springboard (all rights reserved). So there is probably some remaining clooj code in this repo so it isn't completely dead.

vision-05/betterCode: Has remote editing.
