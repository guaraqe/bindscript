module Bindscript.Types
  ( Script (..)
  , Block (..)
  , Data (..)
  , Function (..)
  , Var (..)
  , Auto (..)
  , AutoMode (..)
  , Eff (..)
  ) where

import Data.Text (Text)

-- | Scripts are divided in two parts: a header that is unchanged by the
-- compilation, plus a list of blocks that are to be transformed.
data Script = Script
  { script_header :: Text
  , script_block :: [Block]
  } deriving (Show, Eq)

-- Blocks
data Block
  = BlockData Data
  | BlockFunction Function
  | BlockAuto Auto
  | BlockComment Text
  deriving (Show, Eq)

data Data = Data
  { data_name :: Text
  , data_kind :: Text
  , data_comments :: Text
  } deriving (Show, Eq)

data Function = Function
  { function_name :: Text
  , function_type :: Text
  , function_vars :: [Var]
  , function_js :: Text
  , function_comments :: Text
  } deriving (Show, Eq)

data Var = Empty | Var Text
  deriving (Show, Eq)

data Auto = Auto
  { auto_name :: Text
  , auto_type :: Text
  , auto_mode :: AutoMode
  , auto_comments :: Text
  } deriving (Show, Eq)


data AutoMode
  = AutoPropertyGet Text Int
  | AutoPropertySet Text Int
  | AutoConstructor Eff Text Int Int
  | AutoMethod Eff Text Int Int
  | AutoFunction Eff Text Int Int
  deriving (Show, Eq)

data Eff = Pure | Eff
  deriving (Show, Eq)
