{-# LANGUAGE TemplateHaskell #-}
module InternalTests (tests) where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Data.Map (Map)
import Clif.Basis
import Clif.Internal
import Clif.Arbitrary()

-- * Construction / deconstruction tests

-- fromList . toList == id
prop_toFrom :: Clif (Euclidean Integer) Integer -> Bool
prop_toFrom x = x == (fromList . toList) x

-- toList . fromList == id
-- TODO: Needs fix for unique zero
--prop_fromTo :: [([Euclidean Integer], Integer)] -> Bool
--prop_fromTo x = x == (toList . fromList) x

-- * Properties of the Clif type and instances

-- vec should equal a blade of 1 vector
prop_vec_blade :: Euclidean Integer -> Integer -> Bool
prop_vec_blade e s = vec e s == blade [e] s 

-- Equality should be considered on canonical form for Clifs
prop_eq_on_canon :: Clif (Euclidean Integer) Integer -> Bool
prop_eq_on_canon a = canon a == a

-- canon' should be idempotent (on the implementation level)
prop_canon_idempotent :: Map [Euclidean Integer] Integer -> Bool
prop_canon_idempotent a = canon' (canon' a) == canon' a

-- rev' should be involutive
prop_rev_inv :: Map [Integer] Integer -> Bool
prop_rev_inv a = rev' (rev' a) == a

-- isScalar should be True for these
prop_isScalar_scalar :: Integer -> Bool 
prop_isScalar_scalar a = isScalar $ blade (mempty :: [Euclidean Integer]) a

prop_isScalar_vecProd :: Euclidean Integer -> Integer -> Bool
prop_isScalar_vecProd e s = let v = vec e s in isScalar $ v * v

-- isScalar should be False for these
prop_isScalar_vec :: Euclidean Integer -> NonZero Integer -> Bool
prop_isScalar_vec e (NonZero s) = not . isScalar $ vec e s

-- Template Haskell to generate a TestTree for Tasty from each prop_* property
tests :: TestTree
tests = $(testGroupGenerator)
