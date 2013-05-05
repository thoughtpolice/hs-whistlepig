module Main
       ( main -- :: IO ()
       ) where

import Control.Monad (void, when)
import Control.Monad.Trans.Resource

import Test.Hspec
import Test.HUnit

import Text.Search.Whistlepig
import qualified Text.Search.Whistlepig.Direct as IO

main :: IO ()
main = hspec $ do
  -------------------------------------------------------------------------------
  -- Indexes
  describe "indexes" $ do
    they "can be created" $ runResourceT (void $ create "test")
    they "do exist, unlike unicorns" $ indexExists "test" >>= (@?= True)
    they "can be loaded" $ runResourceT (void $ load "test")
    it "should be empty right now" $ withIndex "test" $ \idx -> do
      sz <- indexSize idx
      when (sz /= 0) $ do
        monadThrow (userError "index size is not zero!")
    they "can be destroyed" $
      deleteIndex "test" >>= errJust "could not delete"

  -------------------------------------------------------------------------------
  -- Entries
  describe "entries" $ return ()

  -------------------------------------------------------------------------------
  -- Queries
  describe "queries" $ do
    they "can be shown" $ do
      x <- IO.stringToQuery "body" "bob ~funny"
      q <- errLeft "Could not create query!" x
      print q

  -------------------------------------------------------------------------------
  -- Searching
  describe "searching" $ return ()

-- helpers
errLeft :: String -> Either a b -> IO b
errLeft x (Left _)  = error x
errLeft _ (Right a) = return a

errJust :: String -> Maybe a -> IO ()
errJust _ Nothing  = return ()
errJust x (Just _) = error x

they :: String -> IO () -> Spec
they = it -- plural
