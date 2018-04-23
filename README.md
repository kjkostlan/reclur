DANGER: Pre-alpha, many features are broken, missing, or incomplete. We plan soon to add the core essential features before making a super-powerful tool.

Reclur is built from the ground up to leverage the power of persistent data structures, the JVM, and clojure's powerful lisp-based macro system. A long time ago, arthuredelstein/clooj was used as a springboard (all rights reserved). But it is hard to find any remaining clooj code at this point.

Persistent Data Structures: With a few possible technical exceptions (such as a java array that is only locally modified in place), the app state is persistent. This makes debugging and adding features easier.

JVM: The JVM is a natural hook for dipping down into high performance array code or interfacing with other languages, which we can more easily use other features with. We use swing but only minimally, so aren't significantly platform-bound.

Macros: Lisp-like languages being homoiconic have a powerful macro system, for which we plan to leverage.

Near term roadmap: Optimize performance (mainly graphics), add core features (find replace, undo).

DANGER: Pre-alpha, many features are broken, missing, or incomplete. Don’t use this yet! oakes/Nightcode is an active, stable IDE for clojure in clojure available now on github.