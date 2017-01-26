{-# LANGUAGE TemplateHaskell #-}
module BasisTests (tests) where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Clif.Basis
import Clif.Arbitrary

-- Utilities

-- Save some characters
type EInt  = Euclidean Integer
--type LInt  = Lorentzian Integer

-- Counts the swaps needed the reverse a list
swaps :: Int -> Int
swaps = (map swaps' [0..] !!)
    where swaps' 0 = 0
          swaps' 1 = 0
          swaps' n = n - 1 + swaps' (n - 1)

-- Properties


-- Properties for orderBasis and orderOrthoBasis

prop_orderBasis_is_freeMul_for_ordered :: Property
prop_orderBasis_is_freeMul_for_ordered = forAll ascLists orderBasis_is_freeMul 

orderBasis_is_freeMul :: ([EInt],[EInt]) -> NonZero Integer -> NonZero Integer -> Bool
orderBasis_is_freeMul (a,b) (NonZero c) (NonZero d) = [freeMul (a, c) (b, d)] == orderBasis [(a,c)] b d

prop_orderOrthoBasis_is_freeMul_for_ordered :: Property
prop_orderOrthoBasis_is_freeMul_for_ordered = forAll ascLists orderOrthoBasis_is_freeMul

orderOrthoBasis_is_freeMul :: ([EInt],[EInt]) -> NonZero Integer -> NonZero Integer -> Bool
orderOrthoBasis_is_freeMul (a,b) (NonZero c) (NonZero d) = freeMul (a, c) (b, d) == orderOrthoBasis a b (c*d)

prop_orderBasis_reverse_ordered_is_nswaps :: Property
prop_orderBasis_reverse_ordered_is_nswaps = forAll ascList orderBasis_reverse_ordered_is_nswaps

orderBasis_reverse_ordered_is_nswaps :: [EInt] -> NonZero Integer -> Bool
orderBasis_reverse_ordered_is_nswaps a (NonZero b) = [(a, (-1)^swaps (length a) * b)] == orderBasis [] (reverse a) b

prop_orderOrthoBasis_reverse_ordered_is_nswaps :: Property
prop_orderOrthoBasis_reverse_ordered_is_nswaps = forAll ascList orderOrthoBasis_reverse_ordered_is_nswaps 

orderOrthoBasis_reverse_ordered_is_nswaps :: [EInt] -> NonZero Integer -> Bool
orderOrthoBasis_reverse_ordered_is_nswaps a (NonZero b) = (a, (-1)^swaps (length a) * b) == orderOrthoBasis [] (reverse a) b


-- Properties for orthoMul and nonOrthoMul

prop_ortho_nonortho_id_diag_metric :: ([EInt], Integer) -> ([EInt], Integer) -> Bool
prop_ortho_nonortho_id_diag_metric a b = 
    filter ((/=(0::Integer)) . snd) [orthoMul a b] == nonOrthoMul a b

-- Template Haskell to generate a TestTree for Tasty from each prop_* property
tests :: TestTree
tests = $(testGroupGenerator)
