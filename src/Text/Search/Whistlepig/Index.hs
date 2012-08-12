-- |
-- Module      : Text.Search.Whistlepig.Index
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Index interface
--
module Text.Search.Whistlepig.Index
       ( -- * Indexes
         Index           -- :: *
       , indexExists     -- :: FilePath -> IO Bool
       , createIndex     -- :: FilePath -> IO (Maybe Index)
       , loadIndex       -- :: FilePath -> IO (Maybe Index)
       , closeIndex      -- :: Index    -> IO ()
       , deleteIndex     -- :: FilePath -> IO ()
       , indexSize       -- :: Index    -> Word64

       , DocID           -- :: *
       , addEntry        -- :: Entry -> Index -> IO (Either Error DocID)
       , addLabel        -- :: Entry -> String -> DocID -> IO (Maybe Error)
       , removeLabel     -- :: Entry -> String -> DocID -> IO (Maybe Error)
       ) where

import Data.Word
import Control.Monad
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc

import Control.Applicative
import Control.Concurrent

import Text.Search.Whistlepig.FFI
import Text.Search.Whistlepig.Entry
import Text.Search.Whistlepig.Util


-------------------------------------------------------------------------------
-- Indexes

newtype Index = Idx (MVar (Ptr WP_Index_t))

newtype DocID = DocID Word64

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
          maybe (Right <$> peek out) (return . Left) e

addEntry :: Index -> Entry -> IO (Either Error DocID)
addEntry (Idx i) (Entry e) =
  withMVar i $ \i' ->
  withMVar e $ \e' ->
  alloca $ \out -> do
    err <- toError =<< c_wp_index_add_entry i' e' out
    maybe (Right . DocID <$> peek out) (return . Left) err

addLabel :: Index -> String -> DocID -> IO (Maybe Error)
addLabel (Idx i) lbl (DocID did) =
  withCString lbl $ \lbl' ->
  withMVar i $ \i' ->
    toError =<< c_wp_index_add_label i' lbl' did

removeLabel :: Index -> String -> DocID -> IO (Maybe Error)
removeLabel (Idx i) lbl (DocID did) =
  withCString lbl $ \lbl' ->
  withMVar i $ \i' ->
    toError =<< c_wp_index_remove_label i' lbl' did


-------------------------------------------------------------------------------
-- Utilities

-- Helper
toIndex :: Ptr WP_Index_t -> IO Index
toIndex ptr = do
    mvar <- newMVar ptr
    addMVarFinalizer mvar (finalize mvar)
    return $! (Idx mvar)
    -- TODO FIXME: should probably handle this somehow?
    -- get rid of 'void!'
  where finalize = flip withMVar (void . c_wp_index_free)
