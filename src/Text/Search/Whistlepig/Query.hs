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

         -- *** Parsing queries
       , stringToQuery -- :: String -> String -> IO (Either Error Query)
       ) where

import Foreign.Ptr
import Foreign.C.String

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

-- | Create a new 'Query' out of a term to search for, and a field
-- it is attached to.
newTermQuery :: String -- ^ Field which word contains
             -> String -- ^ Term to search for
             -> IO Query
newTermQuery field word = do
  q <- withCString field $ \f ->
       withCString word $ \w ->
         c_wp_query_new_term f w
  Query <$> newMVar q

-- | Create a new 'Query' out of a term to search for, and a field
-- it is attached to.
newLabelQuery :: String -- ^ Label to search for
              -> IO Query
newLabelQuery label = do
  q <- withCString label $ \l ->
         c_wp_query_new_label l
  Query <$> newMVar q


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
addQuery (Query q1) (Query q2) = do
  r <- withMVar q1 $ \q1' ->
       withMVar q2 $ \q2' ->
         c_wp_query_add q1' q2'
  Query <$> (newMVar r)

liftQ :: IO (Ptr WP_Query_t) -> IO Query
liftQ k = Query <$> (newMVar =<< k)

-------------------------------------------------------------------------------
-- Parsing

-- TODO FIXME : should have quasiquoting!

-- | Parse a 'String' into a 'Query'.
--
-- If a term in the input query does not have an associated field,
-- then it is attached the field specified in the default field
-- parameter.
stringToQuery :: String -- ^ Query
              -> String -- ^ Default field to assign a term to
              -> IO (Either Error Query)
stringToQuery _query _field = undefined
