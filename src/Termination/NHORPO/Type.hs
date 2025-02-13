-- |types for NHORPO
module Termination.NHORPO.Type where

import Data.Map.Strict (Map)

import Utils.Type (Id(..))

-- |the neutralization level of an argument position of a constant implemented as a map
type NeutralizationLevel = Map (Id,Int) Int

-- |the subset of argument positions used for the neutralization
-- of a given argument of a given constant implemented as a map
type ArgumentPosSubset = Map (Id,Int) [Int]
