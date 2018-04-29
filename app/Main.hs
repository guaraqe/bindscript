{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import Bindscript.Parser
import Bindscript.Javascript
import Bindscript.Purescript

import Control.Exception.Base

import Data.Maybe

import Text.Megaparsec

import Options.Generic

import System.FilePath
import System.Directory
import System.Directory.Tree
  ( DirTree (Failed, Dir, File)
  , AnchoredDirTree (..)
  , readDirectoryWith )

import qualified Data.Text.IO as Text

--------------------------------------------------------------------------------

data Options w = Options
  { input :: w ::: FilePath <?> "Input file or folder"
  , output :: w ::: Maybe FilePath <?> "Output file template or folder"
  } deriving Generic

instance ParseRecord (Options Wrapped)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  Options inputPath outputPath <-
    unwrapRecord "bindscript: FFI generator for Purescript"

  _ :/ tree <- readDirectoryWith (\_ -> pure ()) inputPath

  case tree of

    Failed _ e -> fail $ displayException e

    File _ () -> case outputPath of
      Nothing -> run inputPath (dropExtension inputPath)
      Just path -> run inputPath path

    Dir _ contents ->
      let
        subpaths =
          filter ((==) ".bs" . takeExtension) $
          concatMap filepaths contents

        pathPairs = flip fmap subpaths $ \file ->
          ( inputPath </> file
          , fromMaybe inputPath outputPath </> dropExtension file )
      in
        mapM_ (uncurry run) pathPairs

--------------------------------------------------------------------------------

-- The first input is the bindscript file.
-- The second is the base name of the output files, without extensions.
run :: FilePath -> FilePath -> IO ()
run inputPath outputPath = do
  txt <- Text.readFile inputPath
  case runParser scriptParser inputPath txt of
    Left e -> putStrLn (parseErrorPretty e)
    Right s -> do
      createDirectoryIfMissing True (takeDirectory outputPath)
      Text.writeFile (outputPath <.> "js")   (renderJavascript s)
      Text.writeFile (outputPath <.> "purs") (renderPurescript s)

--------------------------------------------------------------------------------

-- Flatten the directory tree
filepaths :: DirTree a -> [FilePath]
filepaths (Failed _ _) = []
filepaths (File name _) = [name]
filepaths (Dir name l) = (name </>) <$> concatMap filepaths l
