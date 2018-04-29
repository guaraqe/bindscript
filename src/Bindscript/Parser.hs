{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Bindscript.Parser
  ( scriptParser
  , definitionParser
  , dataParser
  , autoParser
  , functionParser
  , commentParser
  , typeDeclarationParser
  , autoModeParser
  , autoPropertyParser
  , jsParser
  , Parser
  ) where

import Bindscript.Types

import Control.Applicative hiding (many, some)
import Control.Monad

import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Void

import Data.Text (Text)
import qualified Data.Text as Text

import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

scriptParser :: Parser Script
scriptParser = do
  h <- Text.pack <$> manyTill anyChar (string "//BINDINGS")
  ds <- many (try (many emptyLine *> definitionParser))
  return (Script h ds)

--------------------------------------------------------------------------------

definitionParser :: Parser Block
definitionParser =
  try (BlockData <$> dataParser) <|>
  try (BlockAuto <$> autoParser) <|>
  try (BlockFunction <$> functionParser) <|>
  (BlockComment <$> commentParser)

--------------------------------------------------------------------------------

dataParser :: Parser Data
dataParser = do
  comments <- option "" commentParser
  string "data"
  someSpace
  (t,k) <- typeDeclarationParser
  return (Data t k comments)

--------------------------------------------------------------------------------

functionParser :: Parser Function
functionParser = do
  comments <- option "" commentParser
  (name, t) <- typeDeclarationParser
  many emptyLine
  vs <- someTill (varParser <* space1) (string "->")
  many (try emptyLine)
  j <- jsParser
  return (Function name t vs j comments)

typeDeclarationParser :: Parser (Text, Text)
typeDeclarationParser = do
  name <- word
  someSpace
  string "::"
  indentation <|> someSpace >> optional indentation
  n <- unPos . sourceColumn <$> getPosition
  line <- takeWhileP Nothing (not . isControl)
  optional eol
  indented <- many indentedLineParser

  let
    t = toBlock $
      if Text.null line
        then EmptyLine : indented
        else IndentedLine (n-1) line : indented

  return (name, t)

--------------------------------------------------------------------------------

autoParser :: Parser Auto
autoParser = do
  comments <- try commentParser <|> pure ""
  (name, t) <- typeDeclarationParser
  many emptyLine
  auto <- try autoModeParser <|> autoPropertyParser
  return (Auto name t auto comments)

autoModeParser :: Parser AutoMode
autoModeParser = do
  auto <- word
  someSpace
  m <- monadicParser
  someSpace
  name <- word
  someSpace
  number <- word
  manySpace
  tryOrNothing eol
  case auto of
    "method" ->
      pure $ AutoMethod m name (read $ Text.unpack number)
    "function" ->
      pure $ AutoFunction m name (read $ Text.unpack number)
    "constructor" ->
      pure $ AutoConstructor m name (read $ Text.unpack number)
    _ ->
      fail "Could not parse Auto"

autoPropertyParser :: Parser AutoMode
autoPropertyParser = do
  _ <- string "property"
  someSpace
  m <- word
  someSpace
  name <- word
  manySpace
  tryOrNothing eol
  case m of
    "get" -> pure $ AutoPropertyGet name
    "set" -> pure $ AutoPropertySet name
    _ -> fail "Could not parse Auto"

monadicParser :: Parser Eff
monadicParser = do
  name <- takeWhile1P Nothing (/= ' ')
  case name of
    "eff" -> return Eff
    "pure" -> return Pure
    _ -> fail "Could not parse eff/pure"

--------------------------------------------------------------------------------

jsParser :: Parser Text
jsParser = toBlock <$> some indentedLineParser

--------------------------------------------------------------------------------

varParser :: Parser Var
varParser = do
  name <- takeWhile1P Nothing (/= ' ')
  if name == "_"
    then return Empty
    else return (Var name)

--------------------------------------------------------------------------------

data IndentedLine = EmptyLine | IndentedLine Int Text
  deriving (Show, Eq)

level :: IndentedLine -> Maybe Int
level EmptyLine = Nothing
level (IndentedLine n _) = Just n

toBlock :: [IndentedLine] -> Text
toBlock l =
  let
    n = minimum (mapMaybe level l)

    toLine EmptyLine = ""
    toLine (IndentedLine m t) = Text.replicate (m - n) " " <> t
  in
    Text.dropWhileEnd (== '\n') $
    Text.dropWhile (== '\n') $
    Text.unlines $
    fmap toLine l

--------------------------------------------------------------------------------

indentedLineParser :: Parser IndentedLine
indentedLineParser = try emptyLine <|> indentedLine

emptyLine :: Parser IndentedLine
emptyLine = do
  manySpace
  eol
  return EmptyLine

indentedLine :: Parser IndentedLine
indentedLine = do
  s <- someSpace
  line <- takeWhile1P Nothing (/= '\n')
  possibleEnd
  return (IndentedLine (length s) line)

possibleEnd :: Parser ()
possibleEnd =
  try (void eol) <|> eof

--------------------------------------------------------------------------------

commentParser :: Parser Text
commentParser =
  let
    commentLine = do
      s <- string "--"
      line <- takeWhileP Nothing (not . isControl)
      optional eol
      return (s <> line)
  in
    fmap
      (Text.dropWhileEnd isControl . Text.unlines)
      (some commentLine)

--------------------------------------------------------------------------------

word :: Parser Text
word = takeWhileP Nothing (\c -> not $ isSeparator c || isControl c)

manySpace :: Parser String
manySpace = many (char ' ')

someSpace :: Parser String
someSpace = some (char ' ')

indentation :: Parser String
indentation = do
  eol
  someSpace

tryOrNothing :: Parser a -> Parser ()
tryOrNothing p = try (p >> pure ()) <|> pure ()
