{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functors: uniform action over a parameterized type, generalizing the
-- 'map' function on lists.

module Data.Functor.Base470 (
    module Data.Functor
#if __GLASGOW_HASKELL__ < 707
  , ($>)
  , void
#endif
) where

#if __GLASGOW_HASKELL__ < 707
import Data.Functor
import Control.Monad

infixl 4 $>

-- | Flipped version of '$>'.
--
-- /Since: 4.7.0.0/
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

#endif
