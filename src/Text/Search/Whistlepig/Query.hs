-- |
-- Module      : Text.Search.Whistlepig.Query
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Query interface
--
module Text.Search.Whistlepig.Query
       ( -- ** Queries
         -- $queries
         Query(..)     -- :: *

         -- *** Constructing queries
       , newTermQuery  -- :: String -> String -> IO Query
       , newLabelQuery -- :: String -> IO Query
       , newConj       -- :: IO Query
       , newDisj       -- :: IO Query
       , newNeg        -- :: IO Query
       , newPhrase     -- :: IO Query
       , newEmpty      -- :: IO Query
       , newEvery      -- :: IO Query
       , addQuery      -- :: Query -> Query -> IO Query
       , cloneQuery    -- :: Query -> IO Query

         -- *** Parsing queries
       , stringToQuery -- :: String -> String -> IO (Either Error Query)
       ) where

import Control.Monad (void)
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.String (withCString, peekCStringLen)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

import Control.Applicative
import Control.Concurrent.MVar

import Text.Search.Whistlepig.FFI
import Text.Search.Whistlepig.Util


-------------------------------------------------------------------------------
-- Queries

-- $queries
-- A query is a ...
--

-- | An 'Index' query.
newtype Query = Query (MVar (Ptr WP_Query_t))
  deriving Eq

instance Show Query where
  show (Query m) = unsafePerformIO $
    withMVar m $ \q ->
    allocaBytes 2048 $ \out -> do
      res <- c_wp_query_to_s q 2048 out
      peekCStringLen (out, fromIntegral res)

-- | Create a new 'Query' out of a term to search for, and a field
-- it is attached to.
newTermQuery :: String -- ^ Field which word contains
             -> String -- ^ Term to search for
             -> IO Query
newTermQuery field word =
  liftQ $ withCString field $ \f ->
          withCString word $ \w ->
            c_wp_query_new_term f w

-- | Create a new 'Query' out of a term to search for, and a field
-- it is attached to.
newLabelQuery :: String -- ^ Label to search for
              -> IO Query
newLabelQuery label =
  liftQ $ withCString label c_wp_query_new_label

newConj :: IO Query
newConj = liftQ c_wp_query_new_conjunction

newDisj :: IO Query
newDisj = liftQ c_wp_query_new_disjunction

newNeg :: IO Query
newNeg = liftQ c_wp_query_new_negation

newPhrase :: IO Query
newPhrase = liftQ c_wp_query_new_phrase

newEmpty :: IO Query
newEmpty = liftQ c_wp_query_new_empty

newEvery :: IO Query
newEvery = liftQ c_wp_query_new_every

-- | Link two 'Query' objects together.
addQuery :: Query -> Query -> IO Query
addQuery (Query q1) (Query q2) =
  liftQ $ withMVar q1 $ \q1' ->
          withMVar q2 $ \q2' ->
            c_wp_query_add q1' q2'

-- | Do a deep clone of a query, dropping all search state.
cloneQuery :: Query -> IO Query
cloneQuery (Query q) = do
  liftQ $ withMVar q c_wp_query_clone

-------------------------------------------------------------------------------
-- Parsing

-- TODO FIXME (#1): should have quasiquoting!

-- | Parse a 'String' into a 'Query'.
--
-- If a term in the input query does not have an associated field,
-- then it is attached the field specified in the default field
-- parameter.
stringToQuery :: String -- ^ Default field to assign a term to
              -> String -- ^ Query
              -> IO (Either Error Query)
stringToQuery field query =
  withCString query $ \q ->
  withCString field $ \f ->
  allocaNull $ \out -> do
    e <- toError =<< c_wp_query_parse q f out
    maybe (toR =<< peek out) (return . Left) e
  where toR o = Right <$> finalizeQuery o

-------------------------------------------------------------------------------
-- Utilities

liftQ :: IO (Ptr WP_Query_t) -> IO Query
liftQ = (>>= finalizeQuery)

finalizeQuery :: Ptr WP_Query_t -> IO Query
finalizeQuery q = do
    p <- newMVar q
    addMVarFinalizer p (finalize p)
    return $! Query p
  -- TODO FIXME (#7): Should not ignore err!
  where finalize = flip withMVar (void . c_wp_query_free)
