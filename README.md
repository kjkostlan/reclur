DANGER: Pre-alpha, many features are broken, missing, or incomplete. We plan soon to add the core essential features before making a super-powerful tool.
 A long time ago, arthuredelstein/clooj was used as a springboard (all rights reserved). But there is little if any remaining clooj code at this point in this repo!

Reclur is built from the ground up to leverage the power of clojure:

Persistent Data Structures: Most of the app state and code is persistent. The exceptions are high-performance functions that internally use java arrays and top-level control flow that uses an atom. But this is far cry from the pervasive mutation behind >95% of the IDE market share. Persistence makes reasoning about the code, debugging, and adding features easier.

JVM: The JVM is a natural hook for dipping down into high performance array code or interfacing with other languages, which we can more easily use other features with. We use swing but only minimally, so aren't strongly platform-bound.

Macros: Lisp-like languages being homoiconic have a powerful macro system and make parsing the code easier.

DANGER: Pre-alpha, many features are broken, missing, or incomplete. Don’t use this yet! oakes/Nightcode is an active, stable IDE for clojure in clojure available now on github.