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
         Error(..)
       , toError
       , allocaNull
       ) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc

import Text.Search.Whistlepig.FFI

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