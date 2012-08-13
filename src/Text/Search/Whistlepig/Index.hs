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
       ( -- ** Indexes
         -- $indexes

         -- *** Managing indexes.
         Index           -- :: *
       , indexExists     -- :: FilePath -> IO Bool
       , createIndex     -- :: FilePath -> IO (Maybe Index)
       , loadIndex       -- :: FilePath -> IO (Maybe Index)
       , closeIndex      -- :: Index    -> IO ()
       , deleteIndex     -- :: FilePath -> IO ()
       , indexSize       -- :: Index    -> IO (Either Error Word64)

         -- *** Adding documents and entries.
       , DocID           -- :: *
       , addEntry        -- :: Entry -> Index -> IO (Either Error DocID)
       , addLabel        -- :: Entry -> String -> DocID -> IO (Maybe Error)
       , removeLabel     -- :: Entry -> String -> DocID -> IO (Maybe Error)

         -- *** Running queries
       , Results         -- :: *
       , runQuery        -- :: Index -> Query -> IO (Either Error Results)
       , countResults    -- :: Index -> Query -> IO (Either Error Word32)
       ) where

import Data.Word
import Control.Monad
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc

import Control.Applicative
import Control.Concurrent

import Control.Monad.IO.Class

import Text.Search.Whistlepig.Entry
import Text.Search.Whistlepig.FFI
import Text.Search.Whistlepig.Query
import Text.Search.Whistlepig.Util


-------------------------------------------------------------------------------
-- Indexes

-- $indexes
-- An 'Index' is a ...
--

-- | A search index.
newtype Index = Idx (MVar (Ptr WP_Index_t))

-- | A @DocID@ describes the position of a document
-- in the sorted search index.
newtype DocID = DocID Word64

-- | Check if an 'Index' exists at a particular @FilePath@
indexExists :: FilePath -- ^ Base path
            -> IO Bool
indexExists = flip withCString $ \path -> do
  err <- c_wp_index_exists path
  return $! if (err /= 0) then True else False

-- | Create an 'Index' on the filesystem with a base path.
createIndex :: FilePath -- ^ Base path
            -> IO (Either Error Index)
createIndex = flip withCString $ \path ->
  allocaNull $ \out -> do
    err <- toError =<< c_wp_index_create out path
    maybe (go out) (return . Left) err
  where
    go out = Right <$> (peek out >>= toIndex)

-- | Load an 'Index' from disk.
loadIndex :: FilePath -- ^ Base path
          -> IO (Either Error Index)
loadIndex = flip withCString $ \path ->
  allocaNull $ \out -> do
    err <- toError =<< c_wp_index_load out path
    maybe (go out) (return . Left) err
  where
    go out = Right <$> (peek out >>= toIndex)

-- | Close an 'Index'.
closeIndex :: Index -- ^ Index
           -> IO (Maybe Error)
closeIndex (Idx i) = withMVar i go
  where go ptr = c_wp_index_unload ptr >>= toError

-- | Delete an 'Index' from the disk.
deleteIndex :: FilePath -- ^ Base path
            -> IO (Maybe Error)
deleteIndex = flip withCString $ \path ->
  c_wp_index_delete path >>= toError

-- | Get the number of documents in an index.
indexSize :: Index -- ^ Index
          -> IO (Either Error Word64)
indexSize (Idx i) = alloca (withMVar i . go)
  where go out ptr = do
          e <- toError =<< c_wp_index_num_docs ptr out
          maybe (Right <$> peek out) (return . Left) e

-- | Add an 'Entry' to the 'Index' and return an 'Error'
-- or the inserted 'DocID'.
addEntry :: Index -- ^ Index
         -> Entry -- ^ Entry
         -> IO (Either Error DocID)
addEntry (Idx i) (Entry e) =
  withMVar i $ \i' ->
  withMVar e $ \e' ->
  alloca $ \out -> do
    err <- toError =<< c_wp_index_add_entry i' e' out
    maybe (Right . DocID <$> peek out) (return . Left) err

-- | Add a label to some particular 'DocID'.
addLabel :: Index  -- ^ Index
         -> String -- ^ Label
         -> DocID  -- ^ Document to add label to
         -> IO (Maybe Error)
addLabel (Idx i) lbl (DocID did) =
  withCString lbl $ \lbl' ->
  withMVar i $ \i' ->
    toError =<< c_wp_index_add_label i' lbl' did

-- | Remove a label from some particular 'DocID'.
removeLabel :: Index  -- ^ Index
            -> String -- ^ Label
            -> DocID  -- ^ Document to remove label from
            -> IO (Maybe Error)
removeLabel (Idx i) lbl (DocID did) =
  withCString lbl $ \lbl' ->
  withMVar i $ \i' ->
    toError =<< c_wp_index_remove_label i' lbl' did

-- | Type of results you get from running a 'Query'.
type Results = [DocID]

-- | Run a 'Query' against an 'Index'.
runQuery :: Index -> Query -> IO (Either Error Results)
runQuery (Idx _i) (Query _q) = error "NIY"

-- | Returns the number of results that match a query. Note
-- this is about as expensive as executing 'runQuery' modulo
-- some extra memory allocations here and there.
countResults :: Index -> Query -> IO (Either Error Word32)
countResults (Idx i) (Query q) =
  alloca $ \out ->
  withMVar i $ \idx ->
  withMVar q $ \query -> do
    -- TODO FIXME: shouldn't ignore err. shouldn't leak at least.
    void $ toError =<< c_wp_index_setup_query idx query
    e <- liftIO $ toError =<< c_wp_index_count_results idx query out
    void $ toError =<< c_wp_index_teardown_query idx query
    maybe (Right <$> peek out) (return . Left) e

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
