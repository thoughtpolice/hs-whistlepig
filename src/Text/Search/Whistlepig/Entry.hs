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
       ( -- * Entries
         Entry(..)  -- :: *
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

newtype Entry = Entry (MVar (Ptr WP_Entry_t))

newEntry :: IO Entry
newEntry = do
    p <- newMVar =<< c_wp_entry_new
    addMVarFinalizer p (finalize p)
    return (Entry p)
    -- TODO FIXME: should probably handle this somehow?
    -- get rid of 'void!'
  where finalize = flip withMVar (void . c_wp_entry_free)

entrySize :: Entry -> IO Word32
entrySize (Entry p) = withMVar p c_wp_entry_size

addToken :: Entry -> String -> String -> IO (Maybe Error)
addToken (Entry e) f t = 
  withCString f $ \field ->
  withCString t $ \term  ->
  withMVar e $ \p        ->
    toError =<< c_wp_entry_add_token p field term

addString :: Entry -> String -> String -> IO (Maybe Error)
addString (Entry e) f s =
  withCString f $ \field ->
  withCString s $ \str   ->
  withMVar e $ \p        ->
    toError =<< c_wp_entry_add_string p field str
