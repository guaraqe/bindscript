{-# LANGUAGE OverloadedStrings #-}

import Bindscript.Parser
import Bindscript.Types

import Data.Either (isRight)
import Data.Text (Text)
import Text.Megaparsec (runParser, parseErrorPretty)

import qualified Data.Text.IO as Text

import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

parse :: Parser a -> Text -> Either String a
parse p t =
  case runParser p "" t of
    Left e -> Left (parseErrorPretty e)
    Right x -> Right x

tests :: TestTree
tests = testGroup "Parsing Tests"
  [ typeDeclaration
  , autoBody
  , js
  , comments
  , script
  ]

--------------------------------------------------------------------------------

typeDeclaration :: TestTree
typeDeclaration = testGroup "Type declaration"
  [
    testCase "Single line without newline" $
      Right ("Name","Type") @=?
      parse typeDeclarationParser "Name :: Type"

  , testCase "Single line with newline" $
      Right ("Name","Type") @=?
      parse typeDeclarationParser "Name :: Type\n"

  , testCase "Many lines without newline" $
      Right ("Name","Type") @=?
      parse typeDeclarationParser "Name ::\n  Type"

  , testCase "Many lines with newline" $
      Right ("Name","Type") @=?
      parse typeDeclarationParser "Name ::\n  Type\n"
  ]

--------------------------------------------------------------------------------

autoBody :: TestTree
autoBody = testGroup "Auto body"
  [
    testCase "Property get without newline" $
      Right (AutoPropertyGet "name") @=?
      parse autoPropertyParser "property get name"

  , testCase "Property set with newline" $
      Right (AutoPropertySet "name") @=?
      parse autoPropertyParser "property set name\n"

  , testCase "Constructor without newline" $
      Right (AutoConstructor Eff "name" 2) @=?
      parse autoModeParser "constructor eff name 2"

  , testCase "Method with newline" $
      Right (AutoMethod Pure "name" 0) @=?
      parse autoModeParser "method pure name 0\n"

  , testCase "Function without newline" $
      Right (AutoFunction Eff "name" 0) @=?
      parse autoModeParser "function eff name 0\n"
  ]

--------------------------------------------------------------------------------

js :: TestTree
js = testGroup "JS body"
  [
    testCase "Without newline" $
      Right "body\nline" @=?
      parse jsParser "  body\n  line"

  , testCase "With newline" $
      Right "body\nline" @=?
      parse jsParser "  body\n  line\n"
  ]

--------------------------------------------------------------------------------

comments :: TestTree
comments = testGroup "Comments"
  [
    testCase "Without newline" $
      Right "-- firstline\n-- | something" @=?
      parse commentParser "-- firstline\n-- | something"
  , testCase "With newline" $
      Right "-- firstline\n-- | something" @=?
      parse commentParser "-- firstline\n-- | something\n"

  ]

--------------------------------------------------------------------------------

script :: TestTree
script = testGroup "Script"
  [
    testCase "Script parser" $ do
      file <- Text.readFile "test/input/teste.bs"
      isRight (parse scriptParser file) @? "Script parsing unsuccessful"
  ]
