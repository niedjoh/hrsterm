-- |simply-typed lambda terms in algebraic representation with fixed arity of function symbols.
-- needed for recursive path orderings.
module ATerm (
  module ATerm.Type,
  module ATerm.Ops,
  module ATerm.BetaEta
) where

import ATerm.Type
import ATerm.Ops
import ATerm.BetaEta
