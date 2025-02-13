{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}

-- |types for NCPO
module Termination.NCPO.Type where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Language.Hasmtlib as SMT

import Utils.Type (Id(..),Accessor(..))
import Utils.RPO (RPOInfo,TypePrecedence,Status,Precedence,ArgOrd)
import Utils.SMT (Constraint)
import ATerm.Type (EId(..))

-- |basic base types
newtype Basic a = Basic (Map Id a) deriving (Foldable,Functor,Traversable)

-- |accessible arguments
newtype Acc a = Acc (Map (EId,Int) a) deriving (Foldable,Functor,Traversable)

-- |small function symbols
newtype Small a = Small (Map EId a) deriving (Foldable,Functor,Traversable)

instance Accessor Basic where
  type AccessorKey Basic = Id
  (Basic m) ! k = m M.! k

instance Accessor Acc where
  type AccessorKey Acc = (EId,Int)
  (Acc m) ! k = m M.! k

instance Accessor Small where
  type AccessorKey Small = EId
  (Small m) ! k = m M.! k

instance SMT.Codec a => SMT.Codec (Basic a) where
  encode = fmap SMT.encode
  decode sol = traverse (SMT.decode sol)

instance SMT.Codec a => SMT.Codec (Acc a) where
  encode = fmap SMT.encode
  decode sol = traverse (SMT.decode sol)

instance SMT.Codec a => SMT.Codec (Small a) where
  encode = fmap SMT.encode
  decode sol = traverse (SMT.decode sol)

-- |RPOInfo, list of basetypes, list of constants, basicMap and accMap bundled into a record data type.
data CPOInfo a b = CPOInfo { rpoInfo :: RPOInfo a b
                           , bTypes :: [Id]
                           , isBasic :: Basic Constraint
                           , isAccessible :: Acc Constraint
                           , isSmall :: Small Constraint
                           }

type CPOSolution = (TypePrecedence Integer, Status ArgOrd, Precedence Integer, Basic Bool, Acc Bool, Small Bool)
