-- |
-- Module      : Text.Search.Whistlepig.IO
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a mid-level FFI binding to the Whistlepig
-- search engine
-- 
module Text.Search.Whistlepig.IO
       ( module Text.Search.Whistlepig.Index
       , module Text.Search.Whistlepig.Query
       , module Text.Search.Whistlepig.Entry
         -- * Error type
       , Error
       ) where
import Text.Search.Whistlepig.Index
import Text.Search.Whistlepig.Query
import Text.Search.Whistlepig.Entry
import Text.Search.Whistlepig.Util (Error)
