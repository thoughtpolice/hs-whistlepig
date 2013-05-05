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
module Text.Search.Whistlepig
       ( -- * Whistlepig search index
         -- $whistlepig
         module Text.Search.Whistlepig.Index
       , module Text.Search.Whistlepig.Query
       , module Text.Search.Whistlepig.Entry
         -- * Error type
       , Error
       ) where
import Text.Search.Whistlepig.Index
import Text.Search.Whistlepig.Query
import Text.Search.Whistlepig.Entry
import Text.Search.Whistlepig.Util (Error)

-------------------------------------------------------------------------------
-- Docs

-- $whistlepig
-- Whistlepig is a minimalist realtime full-text search index. Its goal is to
-- be as small and maintainable as possible, while still remaining useful,
-- performant and scalable to large corpora. If you want realtime full-text
-- search without the frills, Whistlepig may be for you.
--
