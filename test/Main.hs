{-# LANGUAGE PackageImports #-}

module Main (main) where

import "Glob" System.FilePath.Glob (glob)
import "doctest" Test.DocTest (doctest)

main :: IO ()
main = do
  glob "Data/**/*.hs" >>= doctest
  glob "test/**/*.hs" >>= doctest

