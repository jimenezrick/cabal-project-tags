{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Cabal.Plan
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Map as M
import Data.Set as S
import Data.String.Interpolate
import Data.Text (unpack)
import Options.Generic
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process.Typed

data Mode = GhcTagsEmacs | GhcTagsVim | HasktagsEmacs | HasktagsVim deriving (Generic, Show)

instance ParseRecord Mode where
  parseRecord = parseRecordWithModifiers $ lispCaseModifiers {shortNameModifier = firstLetter}

main :: IO ()
main = do
  opts <- getRecord "Caca"
  putStrLn "-- Fetching dependencies"
  findDeps >>= fetchSources
  putStrLn "-- Generating tags"
  generateTags opts

generateTags :: Mode -> IO ()
generateTags mode = do
  root <- findProjectRoot "."
  case root of
    Nothing -> error "cannot find cabal project root"
    Just p -> void . runProcess . setWorkingDir p $ cmd mode
  where
    cmd GhcTagsEmacs = proc "ghc-tags" ["-e", "."]
    cmd GhcTagsVim = proc "ghc-tags" ["-c", "."]
    cmd HasktagsEmacs = proc "hasktags" ["-e", "."]
    cmd HasktagsVim = proc "hasktags" ["-c", "."]

findDeps :: IO [String]
findDeps = do
  plan <- findAndDecodePlanJson $ ProjectRelativeToDir "."
  let locals = [Unit {..} | Unit {..} <- M.elems pm, uType == UnitTypeLocal]
      pm = pjUnits plan
  return
    [ unpack $ dispPkgId pid
      | loItem <- locals,
        (_, cInfo) <- M.toList $ uComps loItem,
        dep <- S.toList $ ciLibDeps cInfo,
        Just pid <- [uPId <$> M.lookup dep pm]
    ]

fetchSources :: [String] -> IO ()
fetchSources deps = do
  root <- findProjectRoot "."
  case root of
    Nothing -> error "cannot find cabal project root"
    Just p -> do
      let depsDir = "cabal-project-deps"
      void ((try $ removeDirectoryRecursive (p </> depsDir)) :: IO (Either SomeException ()))
      createDirectoryIfMissing True depsDir
      void . runProcess $ proc "cabal" (["get", [i|--destdir=#{p </> depsDir}|]] ++ deps)
