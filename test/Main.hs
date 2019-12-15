module Main (main) where

import Test.DocTest (doctest)

main = doctest ["-Iinclude", "Data/MultiSet.hs", "Data/IntMultiSet.hs"]
