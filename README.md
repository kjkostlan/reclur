# 99% pure, functional, immutable clojure editor #

Reclur is functional from the ground up and employs several unique UI design principles.

**Persistent Data Structures:** Mutation is only at the top level or for entities that live outside the app state (files, logging, reloading namespaces). The vast majority of functions are vanilla clojure. Of course, some functions use local atoms or java arrays internally which isn't meaningfully "mutation".

**Hand-rolled GUI:** The entire GUI is vanilla clojure, drawn on a single JPanel. The only interaction with java is receiving events, file I/O, and rendering graphics.

**Clojurized:** Java events such as mousePressed are immediately converted into clojure maps. After that, any function that processes these events is blissfully unaware that Java even exists. Similarly, rendering shapes and text start out as clojure up until the point it gets rendered to java's Graphics2D.

**Large virtual desktop:** Set up many workspaces and pan/zoom your view between them. Move, copy and paste windows or even whole workspaces at once.

**No wasted real-estate:** Line-numbers, scollbars, titlebars, and more take up little space. Windows are slightly transparent which allows getting a sense of what is underneath (hotkey to toggle which is on top) but still shows the top one clearly. Note: scrollbars cannot be dragged, but we use mouse-wheel acceleration to move quickly.

**Logging system:** Put loggers almost anywhere inside of functions to see what is going on. It's easy to add several logger-repls, or add a logger-repl that always points to the last codebox cursor-location you clicked.

**Browse large repl reports:** Have a complicated nested map? Click on the repl's output to zoom in. It can handle infinite data-structures like (range).

**Multiple views:** A single file can have multiple views open, editing one edits all in sync, code-folded or not.

## Currently in alpha ##

We have left pre-alpha with the commit on 2021-Sept-17!

This means that catastrophic bugs with crashing is fixed. Also, basic file-safety such as "really delete?" or "save changes?" is present, but no backups are autosaved so be careful.

A few common features are still missing and minor bugs are fairly common, but we are at least *usable*.

## How to install and use ##

1. Install clojure and [leiningen](https://leiningen.org/).

2. Run the reclur.bat file (windows) or the reclur.sh (mac and linux). It will download a few dependencies.

3. Save your project in src/client/yourProject. Anything in client is .gitignored. Clojure thinks it is all one project, however, so namespaces must be qualified to be in client.yourProject.yourCljFile, and dependencies must be added to the global.lein file. You can add src/startup.clj file which runs on launch and will also be .gitignored.

4. This software is licensed under the [MIT license](https://choosealicense.com/licenses/mit/), with "copyright 2022 Kevin Kostlan" applying to all files in this repo except assets/forest.jpg. This is a [permissive license](https://en.wikipedia.org/wiki/Permissive_software_license), so feel free to use it for either opensource or commercial closed-source products. Just ascribe credit if a significant amount of this codebase ends up in your project. It is my opinion that the open source and commercial worlds should work with each-other with checks-and-balances rather than an either-or dichotomy.

## Or you can check out these other active (~90 days or less) clojure editors (as of fall 2021) ##

**LightTable/LightTable**: Enables *very* real-time coding. Also handles javascript and has an extensive plugin system. This branch is the development branch.

**mogenslund/liquid**: Another attempt to make a pure clojure editor which minimizes mutation. Based out of the terminal.

oakes/Paravim: A vim clojure plugin.

clojure-emacs/cider-nrepl: An emacs must-have for clojure programmers. Very active.

eerohele/Tutkain: A sublime text clojure plugin written in Python. Very active.

PEZ/rich4clojure: A teaching clojure tool from your browser.

Cirru/calcit-editor: Edit clj files like trees instead of as a text file. Trees are stylized with CSS and the parentheses are hidden. The code defines the behavior of various "comps".

arthuredelstein/clooj: The repo itself has been dead since 2014. However, clooj was used as a springboard (all rights reserved). So there is probably some remaining clooj code in this repo so it isn't completely dead.

vision-05/betterCode: Has remote editing.
