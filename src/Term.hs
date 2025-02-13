-- |simply-typed lambda terms
module Term (
  module Term.Type,
  module Term.Ops,
  module Term.BetaEta,
  module Term.Parse
) where

import Term.Type
import Term.Ops
import Term.BetaEta
import Term.Parse
