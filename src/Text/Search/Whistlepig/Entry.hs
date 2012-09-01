-- |
-- Module      : Text.Search.Whistlepig.Entry
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Entry interface
--
module Text.Search.Whistlepig.Entry
       ( -- ** Entries
         -- $entries
         Entry(..)  -- :: *

         -- *** Creating and manipulating entries
       , newEntry   -- :: IO Entry
       , entrySize  -- :: Entry -> IO Word32
       , addToken   -- :: Entry -> String -> String -> IO (Maybe Error)
       , addString  -- :: Entry -> String -> String -> IO (Maybe Error)
       ) where

import Data.Word
import Control.Monad
import Foreign.Ptr
import Foreign.C.String

import Control.Concurrent

import Text.Search.Whistlepig.FFI
import Text.Search.Whistlepig.Util


-------------------------------------------------------------------------------
-- Entries

-- $entries
-- An 'Entry' is a document before it is added to an index. It is a map of
-- @(Field, Term)@ pairs to a sorted list of positions in the index.
--
-- This interface lets you incrementally build up documents in memory before
-- adding them to the index.
--

-- | A document entry.
newtype Entry = Entry (MVar (Ptr WP_Entry_t))

-- | Create a new 'Entry'.
newEntry :: IO Entry
newEntry = do
    p <- newMVar =<< c_wp_entry_new
    addMVarFinalizer p (finalize p)
    return (Entry p)
    -- TODO FIXME (#5): should probably handle this somehow?
    -- get rid of 'void!'
  where finalize = flip withMVar (void . c_wp_entry_free)

-- | Get the size of an 'Entry'.
entrySize :: Entry     -- ^ Entry
          -> IO Word32 -- ^ Output size
entrySize (Entry p) = withMVar p c_wp_entry_size

-- | Add a token to an 'Entry' with a field.
addToken :: Entry  -- ^ Entry
         -> String -- ^ Field
         -> String -- ^ Term
         -> IO (Maybe Error)
addToken (Entry e) f t =
  withCString f $ \field ->
  withCString t $ \term  ->
  withMVar e $ \p        ->
    toError =<< c_wp_entry_add_token p field term

-- | Add a string to an 'Entry' with a field.
addString :: Entry  -- ^ Input entry
          -> String -- ^ Field
          -> String -- ^ String
          -> IO (Maybe Error)
addString (Entry e) f s =
  withCString f $ \field ->
  withCString s $ \str   ->
  withMVar e $ \p        ->
    toError =<< c_wp_entry_add_string p field str
