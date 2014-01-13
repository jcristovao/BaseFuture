{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Foldable
-- Copyright   :  Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of data structures that can be folded to a summary value.
--
-- Many of these functions generalize "Prelude", "Control.Monad" and
-- "Data.List" functions of the same names from lists to any 'Foldable'
-- functor.  To avoid ambiguity, either import those modules hiding
-- these names or qualify uses of these function names with an alias
-- for this module.
--
-----------------------------------------------------------------------------

module Data.Foldable.Base470 (
    module Data.Foldable
  , Foldable(..)
) where

#if __GLASGOW_HASKELL__ < 707
import Data.Foldable
import Data.Monoid (mempty)
import Control.Applicative (Const(..))

instance Foldable (Either a) where
    foldMap _ (Left _) = mempty
    foldMap f (Right y) = f y

    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z

instance Foldable ((,) a) where
    foldMap f (_, y) = f y

    foldr f z (_, y) = f y z

instance Foldable (Const m) where
    foldMap _ _ = mempty
#endif
