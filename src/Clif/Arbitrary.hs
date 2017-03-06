{-|
Module      : Clif.Arbitrary
Copyright   : (c) Matti A. Eskelinen, 2016-2017
License     : MIT
Maintainer  : matti.a.eskelinen@gmail.com
Stability   : experimental
Portability : POSIX

This module provides (orphan) Arbitrary instances and various other generators for creating random 'Clif's and 'Basis' elements using "Test.QuickCheck".

-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Clif.Arbitrary 
    (
    -- * Generators
      ascList, ascLists
    , kBlade
    ) where
import Clif.Basis
import Clif.Internal

import Data.List (nub)
import Test.QuickCheck

-- | Arbitrary instance for 'Euclidean'
instance Arbitrary a => Arbitrary (Euclidean a) where
    arbitrary = E <$> arbitrary
    shrink = genericShrink 

-- | Arbitrary Instance for 'Lorentzian'
instance Arbitrary a => Arbitrary (Lorentzian a) where
    arbitrary = elements [T, S] <*> arbitrary
    shrink = genericShrink

-- | Arbitrary instance for a 'Clif'
instance (Ord b, Arbitrary a, Arbitrary b) => Arbitrary (Clif b a) where
    arbitrary = Clif <$> arbitrary
    shrink = map Clif . shrink . unClif

-- | 'orderedList' with only unique elements. 
-- Useful for generating blades with e.g.
-- 
-- @
-- 'blade' '<$>' 'ascList'
-- @
--
ascList :: (Ord a, Arbitrary a) => Gen [a]
ascList = nub <$> orderedList 

-- | An ascending list split into two at a random point. Useful for generating a pair of blades without common vectors.
ascLists :: (Ord a, Arbitrary a) => Gen ([a], [a])
ascLists = do
    xs    <- ascList
    split <- elements [0..length xs]
    return $ splitAt split xs

-- | Given k, returns a generator for k-blades ('Clif's containing only a single blade of grade k).
kBlade :: (Eq a, Basis b a, Arbitrary a, Arbitrary b) => Int -> Gen (Clif b a)
kBlade k = blade <$> vector k <*> arbitrary

