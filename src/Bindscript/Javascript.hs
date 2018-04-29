{-# LANGUAGE OverloadedStrings #-}

module Bindscript.Javascript
  ( renderJavascript
  ) where

import Bindscript.Types
import Bindscript.Auto

import Data.Maybe
import Data.Monoid (Endo (..))
import Data.Text (Text)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

--------------------------------------------------------------------------------

renderJavascript :: Script -> Text
renderJavascript (Script _ defs) =
  let
    render (BlockData _) = Nothing
    render (BlockComment _) = Nothing
    render (BlockAuto a) = Just (renderFunction (autoToFunction a))
    render (BlockFunction f) = Just (renderFunction f)

    header = pretty ("\"use strict\";" :: Text)

    conc = concatWith (\d1 d2 -> d1 <> line <> line <> d2)
  in
    renderStrict $
    layoutPretty (LayoutOptions Unbounded) $
    conc $
    header : mapMaybe render defs

renderFunction :: Function -> Doc ann
renderFunction (Function name _ [] body _) =
  "exports." <> pretty name <> " =" <> line <>
    indent 2 (pretty body)

renderFunction (Function name _ (v:vs) body _) =
  appEndo
    ( renderLayer ("exports." <> name <> " =") v <>
      foldMap (renderLayer "return") vs
    )
    (pretty body)

renderLayer :: Text -> Var -> Endo (Doc ann)
renderLayer prefix var = Endo $ \body ->
  pretty prefix <> " function(" <> pretty (unVar var) <> ") {" <> line <>
  indent 2 body <> line <>
  "};"

unVar :: Var -> Text
unVar Empty = ""
unVar (Var t) = t
