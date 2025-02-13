{-# LANGUAGE OverloadedStrings #-}

-- |utility functions for prettyprinting
module Utils.Pretty where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Prettyprinter (Doc,Pretty,pretty,parens,vsep,line,(<+>),layoutPretty,defaultLayoutOptions,emptyDoc)
import Prettyprinter.Render.Text (renderStrict,renderLazy)

-- |Prints parents around a document if the given predicate holds.
parensIf :: Bool -> Doc ann -> Doc ann
parensIf p x
  | p         = parens x
  | otherwise = x

-- |Prettyprinting to 'Text'.
prettyText :: Pretty a => a -> Text
prettyText = renderStrict . layoutPretty defaultLayoutOptions . pretty

-- |Prettyprinting to 'Text'.
prettyLazyText :: Pretty a => a -> TL.Text
prettyLazyText = renderLazy . layoutPretty defaultLayoutOptions . pretty

-- |Prettyprinting a rewrite sequence.
prettySequence :: Pretty a => NonEmpty a -> Doc ann
prettySequence (t :| ts)
  | null ts   = emptyDoc
  | otherwise = line <> line <> "  " <+> pretty t <> line <> vsep (map (\s -> "->" <+> pretty s) ts)
