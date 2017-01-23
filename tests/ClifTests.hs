{-# LANGUAGE TemplateHaskell #-}
module ClifTests (tests) where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Clif
import Clif.Arbitrary

-- * Utilities

-- Save some characters
type EClif = Clif (Euclidean Integer) Integer
--type LClif = Clif (Lorentzian Integer) Integer

-- Repeat a constraint for multiple (2 or 3) parameters
forAll2 :: (Show a, Testable b) => Gen a -> (a -> a -> b) -> Property
forAll2 gen f = forAll gen (\a -> forAll gen $ f a)

forAll3 :: (Show a, Testable b) => Gen a -> (a -> a -> a -> b) -> Property
forAll3 gen f = forAll gen (\a -> forAll gen (\b -> forAll gen $ f a b) )

-- Limit the size of Clifs for some tests
sizeLimit :: Int
sizeLimit = 20

smallClif :: Gen (EClif)
smallClif = resize sizeLimit arbitrary

-- * Identities for contractions 
-- See "The inner products of geometric algebra" by Dorst

-- Rewriting rule applies for the left contraction of a wedge product
prop_rewriting_rule :: Int -> Int -> Int -> Property
prop_rewriting_rule k l m = forAll (kBlade k) $ \a -> forAll (kBlade l) $ \b -> forAll (kBlade m) $ rewriting_rule a b

rewriting_rule :: EClif -> EClif -> EClif -> Bool
rewriting_rule u v w = (u `wedge` v) `lContract` w == u `lContract` (v `lContract` w)

-- Sum of contractions equals the sums of scalar and dot product
prop_contractions_eq_scalar_products :: Property 
prop_contractions_eq_scalar_products = forAll2 smallClif contractions_eq_scalar_products

contractions_eq_scalar_products :: EClif -> EClif -> Bool
contractions_eq_scalar_products a b = a `lContract` b + a `rContract` b == a `scalarProd` b + a `dot` b

-- Scalar product of a wedge from the right is the scalar product of a left contraction from the right
prop_lContract_duality :: Property
prop_lContract_duality = forAll3 (smallClif) lContract_duality

lContract_duality :: EClif -> EClif -> EClif -> Bool
lContract_duality u v w = (u `wedge` v) `scalarProd` w == u `scalarProd` (v `lContract` w)

-- Same for the right contraction
prop_rContract_duality :: Property
prop_rContract_duality = forAll3 (smallClif) rContract_duality

rContract_duality :: EClif -> EClif -> EClif -> Bool
rContract_duality u v w = u `scalarProd` (v `wedge` w) == (u `rContract` v) `scalarProd` w

-- Left contraction of reverse clifs is the reverse of a right contraction
prop_revContraction :: Property
prop_revContraction = forAll2 (smallClif) revContraction

revContraction :: EClif -> EClif -> Bool
revContraction u v = rev (u `rContract` v) == rev v `lContract` rev u

-- Scalar product is invariant w.r.t. reversion
prop_scalarProdReversibility :: Property
prop_scalarProdReversibility = forAll2 (smallClif) scalarProdReversibility

scalarProdReversibility :: EClif -> EClif -> Bool
scalarProdReversibility u v = scalarProd u v == scalarProd (rev u) (rev v)

-- Template Haskell to generate a TestTree for Tasty from each prod_* property
tests :: TestTree
tests = $(testGroupGenerator)
