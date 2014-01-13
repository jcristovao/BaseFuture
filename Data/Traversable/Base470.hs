{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of data structures that can be traversed from left to right,
-- performing an action on each element.
--
-- See also
--
--  * \"Applicative Programming with Effects\",
--    by Conor McBride and Ross Paterson,
--    /Journal of Functional Programming/ 18:1 (2008) 1-13, online at
--    <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>.
--
--  * \"The Essence of the Iterator Pattern\",
--    by Jeremy Gibbons and Bruno Oliveira,
--    in /Mathematically-Structured Functional Programming/, 2006, online at
--    <http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator>.
--
--  * \"An Investigation of the Laws of Traversals\",
--    by Mauro Jaskelioff and Ondrej Rypacek,
--    in /Mathematically-Structured Functional Programming/, 2012, online at
--    <http://arxiv.org/pdf/1202.2919>.
--
-- Note that the functions 'mapM' and 'sequence' generalize "Prelude"
-- functions of the same names from lists to any 'Traversable' functor.
-- To avoid ambiguity, either import the "Prelude" hiding these names
-- or qualify uses of these function names with an alias for this module.
--
-----------------------------------------------------------------------------

module Data.Traversable.Base470 (
    module Data.Traversable
  , Traversable(..)
) where

#if __GLASGOW_HASKELL__ < 707
import Data.Traversable
import Data.Foldable.Base470()
import Control.Applicative (Const(..),pure,(<$>))

instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y

instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

instance Traversable (Const m) where
    traverse _ (Const m) = pure $ Const m
#endif
