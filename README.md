DANGER: Pre-alpha, many features are broken, missing, or incomplete. We plan soon to add the core essential features before making a super-powerful tool.

The Cloojzone IDE is built from the ground up to leverage the power of functional programming, the JVM, and lisp’s powerful macro system. 

Making Widgets more functional: Java gives us native access swing’s widgets and functionality, but these are riddled with side effects. We encodes the GUI state as a nested clojure data structure, and bundle the whole thing into an atom. Events are bound to functions (both builtin upkeep and user-defined) that take the state and return a new state (no mutation necessary, but they also have access to the java object if they really need it). The state usually will also include user-defined properties. In addition, this interface is highly simplified: adding and removing children and listeners and changing java objects is automatic. Clojure’s copy-on-modify makes checking equality, etc much cheaper than a naive defensive system, allowing automatically adding updates to be feasible.

Code analyzing code: IDE’s are full of code analysis. Clojure and other Lisp languages excel at analyzing their own code. There is no reason to limit the feature set to the standard fare of refactoring, etc. Although there is very little working code analysis for now, code analysis is planned to become a key strength in the future.

This codebase started as arthuredelstein/clooj (all rights reserved), but the code is substantially modified if not completely overhauled in many places.

DANGER: Pre-alpha, many features are broken, missing, or incomplete. Don’t use this yet! oakes/Nightcode is an active, stable IDE for clojure in clojure available now on github.