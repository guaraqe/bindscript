{-# LANGUAGE OverloadedStrings #-}

module Bindscript.Auto
  ( autoToFunction
  ) where

import Bindscript.Types

import Data.Text (Text)
import Data.Maybe
import Data.Monoid ((<>))

import qualified Data.Text as Text

--------------------------------------------------------------------------------

autoToFunction :: Auto -> Function

autoToFunction (Auto name typ (AutoPropertyGet txt n) comm) =
  let
    vars@(Var v:_) = toVars Pure 0
    dict = dictVars n
  in
    Function name typ (dict <> vars) (propertyGetBody v (replaceName name txt)) comm

autoToFunction (Auto name typ (AutoPropertySet txt n) comm) =
  let
    vars@(Var v: Var r:_) = toVars Eff 1
    dict = dictVars n
  in
    Function name typ (dict <> vars) (propertySetBody v (replaceName name txt) r) comm

autoToFunction (Auto name typ (AutoConstructor mon txt n d) comm) =
  let
    vars = toVars mon (n - 1)
    dict = dictVars d
  in
    Function name typ (dict <> vars) (constructorBody (replaceName name txt) vars) comm

autoToFunction (Auto name typ (AutoMethod mon txt n d) comm) =
  let
    vars@(Var v:vs) = toVars mon n
    dict = dictVars d
  in
    Function name typ (dict <> vars) (methodBody v (replaceName name txt) vs) comm

autoToFunction (Auto name typ (AutoFunction mon txt n d) comm) =
  let
    vars = toVars mon (n - 1)
    dict = dictVars d
  in
    Function name typ (dict <> vars) (functionBody (replaceName name txt) vars) comm

--------------------------------------------------------------------------------

toVars :: Eff -> Int -> [Var]
toVars mon n =
  fmap (\k -> Var ("var" <> Text.pack (show k))) [0..n] ++
  case mon of
    Pure -> []
    Eff -> [Empty]

replaceName :: Text -> Text -> Text
replaceName t "_" = t
replaceName _ t = t


dictVars :: Int -> [Var]
dictVars n =
  fmap (\k -> Var ("dict" <> Text.pack (show k))) [0..n-1]

--------------------------------------------------------------------------------

propertyGetBody :: Text -> Text -> Text
propertyGetBody o p = "return " <> o <> "." <> p <> ";"

propertySetBody :: Text -> Text -> Text -> Text
propertySetBody o p r = o <> "." <> p <> " = " <> r <> ";"

constructorBody :: Text -> [Var] -> Text
constructorBody f vs = "return new " <> f <> argument vs <> ";"

methodBody :: Text -> Text -> [Var] -> Text
methodBody o m vs = "return " <> o <> "." <> m <> argument vs <> ";"

functionBody :: Text -> [Var] -> Text
functionBody f vs = "return " <> f <> argument vs <> ";"

argument :: [Var] -> Text
argument vars =
  let
    f (Var t) = Just t
    f Empty = Nothing
  in
    "(" <> Text.intercalate "," (mapMaybe f vars) <> ")"
