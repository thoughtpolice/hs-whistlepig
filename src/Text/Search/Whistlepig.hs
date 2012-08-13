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
       ( -- * Tutorial
         -- $tutorial

         -- * Interface
         -- ** Index management
         WP.Index
       , WP.indexExists -- :: FilePath -> IO Bool
       , WP.deleteIndex -- :: FilePath -> IO ()
       , create         -- :: MonadResource m => FilePath -> m WP.Index
       , load           -- :: MonadResource m => FilePath -> m WP.Index
       , open           -- :: MonadResource m => FilePath -> m WP.Index
       , withIndex      -- :: FilePath -> (WP.Index -> ResourceT IO a) -> IO a
       , indexSize      -- :: MonadResource m => WP.Index -> m Word64

         -- ** Entry management

         -- ** Queries
         -- *** Quasi quoting
       ) where

import Data.Word
import Control.Applicative

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource

import qualified Text.Search.Whistlepig.IO as WP

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
indexSize idx =   liftIO (WP.indexSize idx)
              >>= onoesIfErr ("Whistlepig.indexSize: could not get size")

-------------------------------------------------------------------------------
-- Entry management


-------------------------------------------------------------------------------
-- Queries


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
