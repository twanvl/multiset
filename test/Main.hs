module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
  glob "Data/**/*.hs" >>= doctest
  glob "test/**/*.hs" >>= doctest

