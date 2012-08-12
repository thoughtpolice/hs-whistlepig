-- |
-- Module      : Text.Search.Whistlepig
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides a high level interface to the Whistlepig C library.
-- 
module Text.Search.Whistlepig
       (
       ) where

import qualified Text.Search.Whistlepig.IO as WP()
import qualified Text.Search.Whistlepig.FFI as FFI()
