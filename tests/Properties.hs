module Main
       ( main -- :: IO ()
       ) where

import Control.Applicative

import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit

import Text.Search.Whistlepig.IO
import Text.Search.Whistlepig.FFI()

main :: IO ()
main = hspec $ do
  -------------------------------------------------------------------------------
  -- Indexes
  describe "indexes" $ do
    they "can be created" $ do
      idx <- createIndex "test" >>= errLeft "could not create"
      closeIndex idx >>= errJust "could not close"
    they "do exist, unlike unicorns" $ do
      r <- indexExists "test"
      r @?= True
    they "can be loaded" $ do
      idx <- loadIndex "test" >>= errLeft "could not load"
      closeIndex idx >>= errJust "could not close"
    they "can be destroyed" $
      deleteIndex "test" >>= errJust "could not delete"

  -------------------------------------------------------------------------------
  -- Entries

  describe "entries" $ return ()

  -------------------------------------------------------------------------------
  -- Queries

  describe "queries" $ return ()


-- helpers
errLeft :: String -> Either a b -> IO b
errLeft x (Left _)  = error x
errLeft _ (Right a) = return a

errJust :: String -> Maybe a -> IO ()
errJust _ Nothing  = return ()
errJust x (Just _) = error x

they = it -- plural
