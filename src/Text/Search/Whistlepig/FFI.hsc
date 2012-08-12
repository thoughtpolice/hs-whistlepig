{-# LANGUAGE CPP #-}
-- |
-- Module      : Text.Search.Whistlepig.FFI
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a low-level FFI binding to the Whistlepig C
-- library.
--
module Text.Search.Whistlepig.FFI
       ( -- * Types
         WP_Err_t, ErrPtr   -- :: *
       , WP_IndexInfo_t     -- :: *
       , WP_Index_t         -- :: *
       , WP_Entry_t         -- :: *

         -- * Errors
       , c_wp_error_free    -- :: Ptr WP_Err_t -> IO ()

         -- * Indexes
       , c_wp_index_exists     -- :: CString -> CInt
       , c_wp_index_create     -- :: Ptr (Ptr WP_Index_t) -> CString -> IO ErrPtr
       , c_wp_index_load       -- :: Ptr (Ptr WP_Index_t) -> CString -> IO ErrPtr
       , c_wp_index_unload     -- :: Ptr WP_Index_t -> IO ErrPtr
       , c_wp_index_free       -- :: Ptr WP_Index_t -> IO ErrPtr
       , c_wp_index_delete     -- :: CString -> IO ErrPtr
       , c_wp_index_num_docs   -- :: Ptr WP_Index_t -> Ptr Word64 -> IO ErrPtr

         -- * Entries
       , c_wp_entry_new        -- :: IO (Ptr WP_Entry_t)
       , c_wp_entry_size       -- :: Ptr WP_Entry_t -> IO Word32
       , c_wp_entry_add_token  -- :: Ptr WP_Entry_t -> CString -> CString -> IO ErrPtr
       , c_wp_entry_add_string -- :: Ptr WP_Entry_t -> CString -> CString -> IO ErrPtr
       , c_wp_entry_free       -- :: Ptr WP_Entry_t -> IO ErrPtr
       ) where

import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <whistlepig/error.h>
#include <whistlepig/index.h>

-- Phantom Types

type ErrPtr = Ptr WP_Err_t

data WP_Err_t
data WP_IndexInfo_t
data WP_Index_t
data WP_Entry_t

--
-- FFI bindings
--

-- Errors

foreign import ccall unsafe "wp_error_free"
  c_wp_error_free :: Ptr WP_Err_t -> IO ()


-- Indexes

foreign import ccall unsafe "wp_index_exists"
  c_wp_index_exists :: CString -> IO CInt

foreign import ccall unsafe "wp_index_create"
  c_wp_index_create :: Ptr (Ptr WP_Index_t) -> CString -> IO ErrPtr

foreign import ccall unsafe "wp_index_load"
  c_wp_index_load :: Ptr (Ptr WP_Index_t) -> CString -> IO ErrPtr

foreign import ccall unsafe "wp_index_unload"
  c_wp_index_unload :: Ptr WP_Index_t -> IO ErrPtr

foreign import ccall unsafe "wp_index_free"
  c_wp_index_free :: Ptr WP_Index_t -> IO ErrPtr

foreign import ccall unsafe "wp_index_delete"
  c_wp_index_delete :: CString -> IO ErrPtr

foreign import ccall unsafe "wp_index_num_docs"
  c_wp_index_num_docs :: Ptr WP_Index_t -> Ptr Word64 -> IO ErrPtr


-- Entries

foreign import ccall unsafe "wp_entry_new"
  c_wp_entry_new :: IO (Ptr WP_Entry_t)

foreign import ccall unsafe "wp_entry_size"
  c_wp_entry_size :: Ptr WP_Entry_t -> IO Word32

foreign import ccall unsafe "wp_entry_add_token"
  c_wp_entry_add_token :: Ptr WP_Entry_t -> CString -> CString -> IO ErrPtr

foreign import ccall unsafe "wp_entry_add_string"
  c_wp_entry_add_string :: Ptr WP_Entry_t -> CString -> CString -> IO ErrPtr

foreign import ccall unsafe "wp_entry_free"
  c_wp_entry_free :: Ptr WP_Entry_t -> IO ErrPtr
