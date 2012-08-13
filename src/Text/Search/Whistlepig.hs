-- |
-- Module      : Text.Search.Whistlepig
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides a high level interface to the Whistlepig C library.
-- 
module Text.Search.Whistlepig
       ( -- * Introduction
         -- $intro

         -- * Tutorial
         -- $tutorial

         -- * Interface
         -- ** Index management
         WP.Index       -- :: *
       , WP.DocID       -- :: *
       , WP.Results     -- :: *
       , WP.indexExists -- :: FilePath -> IO Bool
       , WP.deleteIndex -- :: FilePath -> IO ()
       , create         -- :: MonadResource m => FilePath -> m WP.Index
       , load           -- :: MonadResource m => FilePath -> m WP.Index
       , open           -- :: MonadResource m => FilePath -> m WP.Index
       , withIndex      -- :: FilePath -> (WP.Index -> ResourceT IO a) -> IO a
       , indexSize      -- :: MonadResource m => WP.Index -> m Word64
       , addEntry       -- :: MonadResource m => WP.Index -> WP.Entry -> m WP.DocID
       , addLabel       -- :: MonadResource m => WP.Index -> String -> WP.DocID -> m ()
       , removeLabel    -- :: MonadResource m => WP.Index -> String -> WP.DocID -> m ()

         -- ** Entry management
       , WP.Entry       -- :: *

         -- ** Queries
       , WP.Query       -- :: *
       , runQuery       -- :: MonadResource m => WP.Index -> WP.Query -> m WP.Results
       , countResults   -- :: MonadResource m => WP.Index -> WP.Query -> m Word32
         -- *** Quasi quoting
       ) where

import Data.Word
import Control.Applicative

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource

import qualified Text.Search.Whistlepig.IO as WP

-------------------------------------------------------------------------------
-- Introduction

-- $intro
-- Whistlepig is a minimalist realtime full-text search index. Its goal is to
-- be as small and maintainable as possible, while still remaining useful,
-- performant and scalable to large corpora. If you want realtime full-text
-- search without the frills, Whistlepig may be for you.
--


-------------------------------------------------------------------------------
-- Tutorial

-- $tutorial
-- Lorem ipsum...
--


-------------------------------------------------------------------------------
-- Index management

-- | Creates an 'Index'. If it already exists, this function throws an error.
create :: MonadResource m => FilePath -> m WP.Index
create path = liftIO (WP.indexExists path) >>= bool err create'
  where err = throwErr "Whistlepig.create: index already exists"
        create' = snd <$> allocate (open' path) close'
        open' p =    WP.createIndex p
                 >>= onoesIfErr ("whistlepig: could not open index "++p)
        close' idx =   WP.closeIndex idx
                   >>= onoesIfErr2 ("whistlepig: could not close index "++path)

-- | Load an 'Index'. If it does not exist, this function throws an error.
load :: MonadResource m => FilePath -> m WP.Index
load path = liftIO (WP.indexExists path) >>= bool load' err
  where err = throwErr "Whistlepig.load: index doesn't exist"
        load'   = snd <$> allocate (open' path) close'
        open' p =   WP.loadIndex p
                >>= onoesIfErr ("whistlepig: could not load index "++p)
        close' idx =   WP.closeIndex idx
                   >>= onoesIfErr2 ("whistlepig: could not close index "++path)

-- | Opens an 'Index'. If the index exists, it is opened. If it does not, it
-- is created. If you want to delete an 'Index' before opening it, use
-- 'indexExists' and 'deleteIndex'.
open :: MonadResource m => FilePath -> m WP.Index
open path = liftIO (WP.indexExists path) >>= bool (load path) (create path)

-- | Convenient interface to run a 'ResourceT' over a single index.
withIndex :: FilePath -> (WP.Index -> ResourceT IO a) -> IO a
withIndex path = runResourceT . (open path >>=)

-- | Get the size of an 'Index'.
indexSize :: MonadResource m => WP.Index -> m Word64
indexSize idx
    = liftIO (WP.indexSize idx)
  >>= onoesIfErr ("Whistlepig.indexSize: could not get size")

-- | Add an 'Entry' to the 'Index' and get back the 'DocID'.
addEntry :: MonadResource m => WP.Index -> WP.Entry -> m WP.DocID
addEntry idx entry
    = liftIO (WP.addEntry idx entry)
  >>= onoesIfErr ("Whistlepig.addEntry: could not add entry")

-- | Add a label to some document in the 'Index' via the 'DocID'.
addLabel :: MonadResource m => WP.Index -> String -> WP.DocID -> m ()
addLabel i l d 
    = liftIO (WP.addLabel i l d)
  >>= onoesIfErr2 ("Whistlepig.addLabel: could not add label")

-- | Remove a label from some document in the 'Index' via the 'DocID'.
removeLabel :: MonadResource m => WP.Index -> String -> WP.DocID -> m ()
removeLabel i l d
    = liftIO (WP.addLabel i l d)
  >>= onoesIfErr2 ("Whistlepig.removeLabel: could not rm label")

-------------------------------------------------------------------------------
-- Entry management


-------------------------------------------------------------------------------
-- Queries

-- | Run a 'Query' against an 'Index'. The result is a list of 'DocID's that
-- the query matches.
runQuery :: MonadResource m => WP.Index -> WP.Query -> m WP.Results
runQuery idx q
    = liftIO (WP.runQuery idx q)
  >>= onoesIfErr ("Whistlepig.runQuery: could not run query")

-- | Returns the number of results that match a query. Note
-- this is about as expensive as executing 'runQuery' modulo
-- some extra memory allocations here and there.
countResults :: MonadResource m => WP.Index -> WP.Query -> m Word32
countResults idx q
    = liftIO (WP.countResults idx q)
  >>= onoesIfErr ("Whistlepig.countResults: could not get result count")

-------------------------------------------------------------------------------
-- Utilities

bool :: a -> a -> Bool -> a
bool y n b = if b then y else n

onoesIfErr :: (MonadThrow m, Show e) => String -> Either e a -> m a
onoesIfErr msg = either (throwUserErr msg) return

onoesIfErr2 :: (MonadThrow m, Show e) => String -> Maybe e -> m ()
onoesIfErr2 msg = maybe (return ()) (throwUserErr msg)

throwUserErr :: (MonadThrow m, Show e) => String -> e -> m a
throwUserErr msg e = monadThrow (userError $ msg ++ ": " ++ show e)

throwErr :: MonadThrow m => String -> m a
throwErr = monadThrow . userError
