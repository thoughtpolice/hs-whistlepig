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
         WP_Err_t, ErrPtr      -- :: *
       , WP_IndexInfo_t        -- :: *
       , WP_Index_t            -- :: *
       , WP_Entry_t            -- :: *
       , WP_Query_t            -- :: *
         -- * Errors
       , c_wp_error_free       -- :: Ptr WP_Err_t -> IO ()

         -- * Indexes
       , c_wp_index_exists     -- :: CString -> CInt
       , c_wp_index_create     -- :: Ptr (Ptr WP_Index_t) -> CString -> IO ErrPtr
       , c_wp_index_load       -- :: Ptr (Ptr WP_Index_t) -> CString -> IO ErrPtr
       , c_wp_index_unload     -- :: Ptr WP_Index_t -> IO ErrPtr
       , c_wp_index_free       -- :: Ptr WP_Index_t -> IO ErrPtr
       , c_wp_index_delete     -- :: CString -> IO ErrPtr
       , c_wp_index_num_docs   -- :: Ptr WP_Index_t -> Ptr Word64 -> IO ErrPtr
       , c_wp_index_add_entry      -- :: ...
       , c_wp_index_add_label      -- :: ...
       , c_wp_index_remove_label   -- :: ...
       , c_wp_index_setup_query    -- :: Ptr WP_Index_t -> Ptr WP_Query_t -> IO ErrPtr
       , c_wp_index_teardown_query -- :: Ptr WP_Index_t -> Ptr WP_Query_t -> IO ErrPtr
       , c_wp_index_run_query      -- :: ...
       , c_wp_index_count_results  -- :: ...

         -- * Entries
       , c_wp_entry_new        -- :: IO (Ptr WP_Entry_t)
       , c_wp_entry_size       -- :: Ptr WP_Entry_t -> IO Word32
       , c_wp_entry_add_token  -- :: Ptr WP_Entry_t -> CString -> CString -> IO ErrPtr
       , c_wp_entry_add_string -- :: Ptr WP_Entry_t -> CString -> CString -> IO ErrPtr
       , c_wp_entry_free       -- :: Ptr WP_Entry_t -> IO ErrPtr

         -- * Queries
       , c_wp_query_new_term        -- :: CString -> CString -> IO (Ptr WP_Query_t)
       , c_wp_query_new_label       -- :: CString -> IO (Ptr WP_Query_t)
       , c_wp_query_new_conjunction -- :: IO (Ptr WP_Query_t)
       , c_wp_query_new_disjunction -- :: IO (Ptr WP_Query_t)
       , c_wp_query_new_negation    -- :: IO (Ptr WP_Query_t)
       , c_wp_query_new_phrase      -- :: IO (Ptr WP_Query_t)
       , c_wp_query_new_empty       -- :: IO (Ptr WP_Query_t)
       , c_wp_query_new_every       -- :: IO (Ptr WP_Query_t)
       , c_wp_query_clone           -- :: Ptr WP_Query_t -> IO (Ptr WP_Query_t)
       , c_wp_query_addd            -- :: ... -> IO (Ptr WP_Query_t)
       , c_wp_query_free            -- :: Ptr WP_Query_t -> IO ErrPtr
       , c_wp_query_to_s            -- :: ...-> IO (Ptr WP_Query_t)
       , c_wp_query_parse -- :: CString -> CString -> (Ptr (Ptr WP_Query_t)) -> IO ErrPtr
         -- * Searching
       ) where

import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <whistlepig/error.h>
#include <whistlepig/index.h>
#include <whistlepig/entry.h>
#include <whistlepig/query.h>
#include <whistlepig/query-parser.h>
#include <whistlepig/search.h>

-- Phantom Types

-- | Convenient alias
type ErrPtr = Ptr WP_Err_t

data WP_Err_t
data WP_IndexInfo_t
data WP_Index_t
data WP_Entry_t
data WP_Query_t

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

foreign import ccall unsafe "wp_index_add_entry"
  c_wp_index_add_entry :: Ptr WP_Index_t -> Ptr WP_Entry_t -> Ptr Word64 -> IO ErrPtr

foreign import ccall unsafe "wp_index_add_label"
  c_wp_index_add_label :: Ptr WP_Index_t -> CString -> Word64 -> IO ErrPtr

foreign import ccall unsafe "wp_index_remove_label"
  c_wp_index_remove_label :: Ptr WP_Index_t -> CString -> Word64 -> IO ErrPtr

-- | Must be called BEFORE 'c_wp_index_run_query'!
foreign import ccall unsafe "wp_index_setup_query"
  c_wp_index_setup_query :: Ptr WP_Index_t -> Ptr WP_Query_t -> IO ErrPtr

-- | Must be called AFTER 'c_wp_index_run_query'!
foreign import ccall unsafe "wp_index_teardown_query"
  c_wp_index_teardown_query :: Ptr WP_Index_t -> Ptr WP_Query_t -> IO ErrPtr

foreign import ccall unsafe "wp_index_run_query"
  c_wp_index_run_query :: Ptr WP_Index_t
                       -> Ptr WP_Query_t
                       -> Word32
                       -> Ptr Word32
                       -> Ptr Word64
                       -> IO ErrPtr

foreign import ccall unsafe "wp_index_count_results"
  c_wp_index_count_results :: Ptr WP_Index_t
                           -> Ptr WP_Query_t
                           -> Ptr Word32
                           -> IO ErrPtr


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


-- Queries

foreign import ccall unsafe "wp_query_new_term"
  c_wp_query_new_term :: CString -> CString -> IO (Ptr WP_Query_t)

foreign import ccall unsafe "wp_query_new_label"
  c_wp_query_new_label :: CString -> IO (Ptr WP_Query_t)

foreign import ccall unsafe "wp_query_new_conjunction"
  c_wp_query_new_conjunction :: IO (Ptr WP_Query_t)

foreign import ccall unsafe "wp_query_new_disjunction"
  c_wp_query_new_disjunction :: IO (Ptr WP_Query_t)

foreign import ccall unsafe "wp_query_new_negation"
  c_wp_query_new_negation :: IO (Ptr WP_Query_t)

foreign import ccall unsafe "wp_query_new_phrase"
  c_wp_query_new_phrase :: IO (Ptr WP_Query_t)

foreign import ccall unsafe "wp_query_new_empty"
  c_wp_query_new_empty :: IO (Ptr WP_Query_t)

foreign import ccall unsafe "wp_query_new_every"
  c_wp_query_new_every :: IO (Ptr WP_Query_t)

foreign import ccall unsafe "wp_query_clone"
  c_wp_query_clone :: Ptr WP_Query_t -> IO (Ptr WP_Query_t)

{--
foreign import ccall unsafe "wp_query_substitute"
  c_wp_query_new_ :: ... -> IO (Ptr WP_Query_t)
--}

foreign import ccall unsafe "wp_query_add"
  c_wp_query_addd :: Ptr WP_Query_t -> IO (Ptr WP_Query_t)

foreign import ccall unsafe "wp_query_free"
  c_wp_query_free :: Ptr WP_Query_t -> IO ErrPtr

foreign import ccall unsafe "wp_query_to_s"
  c_wp_query_to_s :: Ptr WP_Query_t -> CSize -> CString -> IO CSize

foreign import ccall unsafe "wp_query_parse"
  c_wp_query_parse :: CString -> CString -> Ptr (Ptr WP_Query_t) -> IO ErrPtr
