{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module describes a structure intermediate between a functor and
-- a monad (technically, a strong lax monoidal functor).  Compared with
-- monads, this interface lacks the full power of the binding operation
-- '>>=', but
--
-- * it has more instances.
--
-- * it is sufficient for many uses, e.g. context-free parsing, or the
--   'Data.Traversable.Traversable' class.
--
-- * instances can perform analysis of computations before they are
--   executed, and thus produce shared optimizations.
--
-- This interface was introduced for parsers by Niklas R&#xF6;jemo, because
-- it admits more sharing than the monadic interface.  The names here are
-- mostly based on parsing work by Doaitse Swierstra.
--
-- For more details, see /Applicative Programming with Effects/,
-- by Conor McBride and Ross Paterson, online at
-- <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>.

module Control.Applicative.Compat (
#if !MIN_VERSION_base(4,7,0)
  , Const(..)
  , WrappedMonad(..)
#endif
    ) where

#if !MIN_VERSION_base(4,7,0)
import Control.Applicative
import Data.Monoid (Monoid(..))

-- Added in base-4.7.0.0
instance Monoid a => Monoid (Const a b) where
    mempty = Const mempty
    mappend (Const a) (Const b) = Const (mappend a b)

-- Added in base-4.7.0.0 (GHC Trac #8218)
instance Monad m => Monad (WrappedMonad m) where
    return = WrapMonad . return
    a >>= f = WrapMonad (unwrapMonad a >>= unwrapMonad . f)
#endif
