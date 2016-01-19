{-# LANGUAGE PackageImports #-}

module Main (main) where

import "Glob" System.FilePath.Glob (glob)
import "doctest" Test.DocTest (doctest)

includeDirs :: [String]
includeDirs = ["include"]

doctestWithIncludeDirs :: [String] -> IO ()
doctestWithIncludeDirs fs = doctest (map ((++) "-I") includeDirs ++ fs)

main :: IO ()
main = do
  glob "Data/**/*.hs" >>= doctestWithIncludeDirs
  glob "test/**/*.hs" >>= doctestWithIncludeDirs

