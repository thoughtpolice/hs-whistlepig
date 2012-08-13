-- |
-- Module      : Text.Search.Whistlepig.Util
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Index interface
--
module Text.Search.Whistlepig.Util
       ( -- * Errors
         Error(..)  -- :: *
       , toError    -- :: Ptr WP_Err_t -> IO (Maybe Error)
       , allocaNull -- :: (Ptr (Ptr a) -> IO b) -> IO b
       ) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc

import Text.Search.Whistlepig.FFI

-------------------------------------------------------------------------------
-- Errors

-- TODO FIXME: expand ability to get error information

-- | The @Error@ type is returned by Whistlepig functions to indicate errors
-- somewhere.
newtype Error = Error (ForeignPtr WP_Err_t)
  deriving Eq

instance Show Error where
  show _ = "ERROR!" -- TODO FIXME: obvious

-- | Convenient wrapper that adds a finalizer to the 'Error' pointer if
-- it is needed so it will get freed.
toError :: Ptr WP_Err_t -> IO (Maybe Error)
toError ptr
  | ptr == nullPtr = return Nothing
  | otherwise = do
    p <- newForeignPtr ptr (c_wp_error_free ptr)
    return (Just $ Error p)

-------------------------------------------------------------------------------
-- Utilities

-- | Allocate a pointer to a pointer and set it to @NULL@ to initialize it.
allocaNull :: (Ptr (Ptr a) -> IO b) -> IO b
allocaNull f = alloca $ \p -> poke p nullPtr >> f p
