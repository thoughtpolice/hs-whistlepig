module Main
       ( main -- :: IO ()
       ) where

import Test.Hspec
import Test.Hspec.HUnit()
import Test.QuickCheck()

main :: IO ()
main = hspec $ do
  describe "test" $ do
    it "does something dumb" $ 1 == (1::Int)
  describe "test2" $ do
    it "does something else" $ 2 == (2::Int)
