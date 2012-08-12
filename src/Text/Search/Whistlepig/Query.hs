-- |
-- Module      : Text.Search.Whistlepig.Query
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Query interface
--
module Text.Search.Whistlepig.Query
       ( -- * Queries
         Query    -- :: *
       ) where

{--}
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
import Text.Search.Whistlepig.Util
--}


-------------------------------------------------------------------------------
-- Queries

newtype Query = Query (MVar (Ptr WP_Query_t))
