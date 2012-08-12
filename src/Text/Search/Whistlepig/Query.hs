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
       ( -- ** Queries
         -- $queries
         Query         -- :: *

         -- *** Parsing queries
       , stringToQuery -- :: String -> String -> IO (Either Error Query)
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

-- $queries
-- A query is a ...
--

-- | An 'Index' query.
newtype Query = Query (MVar (Ptr WP_Query_t))

-- | Parse a 'String' into a 'Query'.
--
-- If a term in the input query does not have an associated field,
-- then it is attached the field specified in the default field
-- parameter.
stringToQuery :: String -- ^ Query
              -> String -- ^ Default field to assign a term to
              -> IO (Either Error Query)
stringToQuery _query _field = undefined
