-- |
-- Module      : Text.Search.Whistlepig.IO
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a mid-level FFI binding to the Whistlepig
-- search engine
-- 
module Text.Search.Whistlepig.IO
       ( -- * Indexes
         Index
       , indexExists     -- :: FilePath -> IO Bool
       , createIndex     -- :: FilePath -> IO (Maybe Index)
       , loadIndex       -- :: FilePath -> IO (Maybe Index)
       , closeIndex      -- :: Index    -> IO ()
       , deleteIndex     -- :: FilePath -> IO ()
       , indexSize       -- :: Index    -> Word64
         -- * Entries
         -- * Queries

         -- * Errors
       , Error
       ) where


import Data.Word
import Control.Monad
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar

import Text.Search.Whistlepig.FFI


-------------------------------------------------------------------------------
-- Indexes

newtype Index = Idx (MVar (Ptr WP_Index_t))

-- | Check if an index exists at a particular @FilePath@
indexExists :: FilePath -> IO Bool
indexExists = flip withCString $ \path -> do
  err <- c_wp_index_exists path
  return $! if (err /= 0) then True else False

-- | Create an index on the filesystem with a base path.
createIndex :: FilePath -> IO (Either Error Index)
createIndex = flip withCString $ \path ->
  allocaNull $ \out -> do
    err <- toError =<< c_wp_index_create out path
    maybe (go out) (return . Left) err
  where
    go out = Right <$> (peek out >>= toIndex)

-- | Load an index from disk.
loadIndex :: FilePath -> IO (Either Error Index)
loadIndex = flip withCString $ \path ->
  allocaNull $ \out -> do
    err <- toError =<< c_wp_index_load out path
    maybe (go out) (return . Left) err
  where
    go out = Right <$> (peek out >>= toIndex)

-- Helper
toIndex :: Ptr WP_Index_t -> IO Index
toIndex ptr = do
    mvar <- newMVar ptr
    addMVarFinalizer mvar (finalize mvar)
    return $! (Idx mvar)
    -- TODO FIXME: should probably handle this somehow?
    -- get rid of 'void!'
  where finalize = flip withMVar (void . c_wp_index_free)

-- | Close an Index.
closeIndex :: Index -> IO (Maybe Error)
closeIndex (Idx i) = withMVar i go
  where go ptr = c_wp_index_unload ptr >>= toError

-- | Delete an index from the disk.
deleteIndex :: FilePath -> IO (Maybe Error)
deleteIndex = flip withCString $ \path ->
  c_wp_index_delete path >>= toError

-- | Get the number of documents in an index.
indexSize :: Index -> IO (Either Error Word64)
indexSize (Idx i) = alloca (withMVar i . go)
  where go out ptr = do
          e <- toError =<< c_wp_index_num_docs ptr out
          maybe (Right <$> copy out) (return . Left) e
        copy p = fromIntegral <$> peek p

-------------------------------------------------------------------------------
-- Errors

newtype Error = Error (ForeignPtr WP_Err_t)

toError :: Ptr WP_Err_t -> IO (Maybe Error)
toError ptr
  | ptr == nullPtr = return Nothing
  | otherwise = do
    p <- newForeignPtr ptr (c_wp_error_free ptr)
    return (Just $ Error p)

-------------------------------------------------------------------------------
-- Utilities

allocaNull :: (Ptr (Ptr a) -> IO b) -> IO b
allocaNull f = alloca $ \p -> poke p nullPtr >> f p
