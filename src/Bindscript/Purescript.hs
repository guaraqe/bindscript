{-# LANGUAGE OverloadedStrings #-}

module Bindscript.Purescript
  ( renderPurescript
  ) where

import Bindscript.Types
import Bindscript.Auto

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

--------------------------------------------------------------------------------

renderPurescript :: Script -> Text
renderPurescript (Script header defs) =
  let
    conc = concatWith (\d1 d2 -> d1 <> line <> line <> d2)
  in
    renderStrict $
    layoutPretty (LayoutOptions Unbounded) $
    pretty header <> conc (fmap renderBlock defs)

renderBlock :: Block -> Doc ann
renderBlock (BlockData a) = renderData a
renderBlock (BlockFunction a) = renderFunction a
renderBlock (BlockComment a) = pretty a
renderBlock (BlockAuto a) = renderBlock (BlockFunction (autoToFunction a))

renderData :: Data -> Doc ann
renderData (Data name kind comments) =
  renderComments comments <>
  "foreign import data " <> pretty name <> " ::" <>
    if length (Text.lines kind) == 1
      then " " <> pretty kind
      else line <> indent 2 (pretty kind)

renderFunction :: Function -> Doc ann
renderFunction (Function name typ _ _ comments) =
  renderComments comments <>
  "foreign import " <> pretty name <> " ::" <>
    if length (Text.lines typ) == 1
      then " " <> pretty typ
      else line <> indent 2 (pretty typ)

renderComments :: Text -> Doc ann
renderComments "" = ""
renderComments comments = pretty comments <> line
