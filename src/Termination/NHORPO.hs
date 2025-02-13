-- |normal higher-order recursive path order as presented in Jouannaud & Rubio's 2015 article
-- <https://doi.org/10.1145/2699913>
module Termination.NHORPO (
  module Termination.NHORPO.Type,
  module Termination.NHORPO.Neutralization,
  module Termination.NHORPO.Ordering,
  module Termination.NHORPO.Solver
) where

import Termination.NHORPO.Type
import Termination.NHORPO.Neutralization
import Termination.NHORPO.Ordering
import Termination.NHORPO.Solver
