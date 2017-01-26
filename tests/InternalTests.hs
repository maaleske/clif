{-# LANGUAGE TemplateHaskell #-}
module InternalTests (tests) where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Data.Map (Map)
import Clif.Basis
import Clif.Internal
import Clif.Arbitrary()

-- * Properties of the Clif type and instances

-- Equality should be considered on canonical form for Clifs
prop_eq_on_canon :: Clif (Euclidean Integer) Integer -> Bool
prop_eq_on_canon a = canon a == a

-- canon' should be idempotent (on the implementation level)
prop_canon_idempotent :: Map [Euclidean Integer] Integer -> Bool
prop_canon_idempotent a = canon' (canon' a) == canon' a

-- rev' should be involutive
prop_rev_inv :: Map [Integer] Integer -> Bool
prop_rev_inv a = rev' (rev' a) == a

-- Template Haskell to generate a TestTree for Tasty from each prop_* property
tests :: TestTree
tests = $(testGroupGenerator)
