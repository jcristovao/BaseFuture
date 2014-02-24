{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bool
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'Bool' type and related functions.
--
-----------------------------------------------------------------------------

module Data.Bool.Compat (
    module Data.Bool
#if __GLASGOW_HASKELL__ < 707
  , bool
#endif
) where

#if __GLASGOW_HASKELL__ < 707
import Data.Bool

-- | Case analysis for the 'Bool' type.
-- @bool a b p@ evaluates to @a@ when @p@ is @False@, and evaluates to @b@
-- when @p@ is @True@.
--
-- /Since: 4.7.0.0/
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t

#endif
