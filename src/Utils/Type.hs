{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |utility type declarations
module Utils.Type where

import Data.Text (Text)
import Prettyprinter (Pretty)

-- |newtype wrapper for 'Text'-based identifiers
newtype Id = Id Text deriving (Eq,Ord,Show,Pretty)

-- |type class which lifts the map accessor function accordingly
class Accessor a where
  type AccessorKey a
  (!) :: a b -> AccessorKey a -> b

infixl 9 !
