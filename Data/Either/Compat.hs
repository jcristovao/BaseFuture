{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Either type, and associated operations.
--
-----------------------------------------------------------------------------

module Data.Either.Compat (
  module Data.Either
#if __GLASGOW_HASKELL__ < 707
  , isLeft
  , isRight
#endif
 ) where

#if __GLASGOW_HASKELL__ < 707
import Data.Either

-- | Return `True` if the given value is a `Left`-value, `False` otherwise.
--
-- /Since: 4.7.0.0/
isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

-- | Return `True` if the given value is a `Right`-value, `False` otherwise.
--
-- /Since: 4.7.0.0/
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True
#endif
