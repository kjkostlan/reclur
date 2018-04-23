; ************************************************************************
; ************************************************************************
; WARNING: This WILL be refactored away, protocols are just to circular in their dependencies.
; ************************************************************************
; ************************************************************************

(ns coder.rcodeprotocol)

(defprotocol TwoWayParser
 "The language functions needed to make a two way parser."
  (character-tokenize-groups [this])
  (token-matchers [this])
  (non-bracket-group [this x])
  (meta-assign [this xs xt])
  (type-of-group [this outer-strings x])
  (meta-parse [this x])
  (leaf-parse [this s s-sp ty])
  (readermacro-apply [this x])
  (fnfirst-order [this x])
  (fnfirst-unorder [this x])
  (readermacro-unapply [this x pre-h pre-t])
  (meta-unparse [this x r])
  (non-bracket-ungroup [this x])
  (coll-project [this x0 x0-tstrs x outer-strs meta?s ty outer-level?])
  (leaf-project [this x r rmacro?]))