{-|
Module      : Clif.Internals
Copyright   : (c) Matti A. Eskelinen, 2016
License     : MIT
Maintainer  : matti.a.eskelinen@gmail.com
Stability   : experimental
Portability : POSIX


This module implements the various 'Clif' operations on the underlying type. Currently 'Clif' is implemented on top of "Data.Map", with blades as keys and scalar multipliers as values.

= Warning
__This module is not intended to be imported by end users and may change drastically in the future. It is currently exposed (and documented) only to help development.__

As development continues, some of the definitions here may be exported from the other modules. Comments and suggestions are welcomed.

-}
{-# LANGUAGE
    Safe
  , DeriveFunctor
  #-} 
{-# OPTIONS_HADDOCK not-home #-}
module Clif.Internal where

import Clif.Basis

import Control.Applicative
import qualified Data.Map as M
import Data.List (nub, intercalate)
import Data.Map (Map)
import Data.Function (on)

-- * Type @Clif@

-- | A data type representing a Clif (multivector) composed of direct sum of scaled blades
newtype Clif b a = Clif {unClif :: Map [b] a}
    deriving (Functor)

-- * Constructors

-- | Constructs a 'Clif' from a list of blades and their multipliers in canonical form.
--
-- >>> fromList [([], 42), ([E 1, E 2], 1)]
-- 42 *: [] + 1 *: [E 1,E 2]
fromList :: (Eq a, Basis b a) => [([b], a)] -> Clif b a
fromList = Clif . canon' . M.fromListWith (+)

-- | Constructor for a blade 
blade :: (Eq a, Basis b a) => [b] -> a -> Clif b a
blade = Clif .: M.singleton

-- | Infix synonym for @'flip' 'blade'@
infix 9 *:
(*:) :: (Eq a, Basis b a) => a -> [b] -> Clif b a
(*:) = flip blade 

-- | Constructor for basis vector values. 
-- Note that @'vec' a s@ is equivalent to @'blade' [a] s@.
vec :: (Eq a, Basis b a) => b -> a -> Clif b a
vec = Clif .: M.singleton . (:[])

-- | 'Show' instance just shows the underlying 'Map' for now
instance (Show b, Show a) => Show (Clif b a) where
    show = intercalate " + " . map showBlade . M.assocs . unClif
             where showBlade (b, s) = show s ++ " *: " ++ show b

-- | The Eq instance calculates the canonical forms of the compared Clifs before comparison.
instance (Eq b, Eq a, Basis b a) => Eq (Clif b a) where
    (==) = (==) `on` (unClif . canon)

-- | Note that abs and signum are only well-defined on the scalar component of each Clif, and zero otherwise.
instance (Eq a, Basis b a) => Num (Clif b a) where
    (+) = gPlus
    (*) = gMul
    abs = fmap abs . grade 0
    negate = fmap negate 
    signum = fmap signum . grade 0
    fromInteger = blade mempty . fromInteger

-- | Inverse elements only exist for Clifs c for which c times c is scalar. For others, recip does not terminate.
instance (Eq a, Basis b a, Fractional a) => Fractional (Clif b a) where
    fromRational = blade mempty . fromRational
    recip v | isScalar v = fmap recip v
            | otherwise  = revM . recip $ revM v
                where revM = gMul (rev v)

-- * Deconstruction

-- | Return a list of the blades and coefficients of a Clif. Note that 'toList' is not an inverse of 'fromList' due to nonuniquess of zeros in the current implementation.
toList :: Clif b a -> [([b], a)]
toList = M.toList . unClif

-- * Operations

-- | The Clifford (geometric) product on 'Clif's.
gMul :: (Eq a, Basis b a) => Clif b a -> Clif b a -> Clif b a
gMul = Clif .: gMul' `on` unClif

-- | The Clifford product on 'Map's of blades and multipliers. Filter out zero values
gMul' :: (Eq a, Basis b a) => Map [b] a -> Map [b] a -> Map [b] a
gMul' = M.filter (/= 0) . M.fromListWith (+) . concat .: liftA2 basisMul `on` M.assocs

-- | Addition of 'Clif' values (direct sum).
gPlus :: (Eq a, Basis b a) => Clif b a -> Clif b a -> Clif b a
gPlus = Clif .: gPlus' `on` unClif

-- | Direct sum of matching keys from two 'Map's. Filter out zero values.
gPlus' :: (Eq a, Basis b a) => Map [b] a -> Map [b] a -> Map [b] a
gPlus' = M.filter (/= 0) .: M.unionWith (+)

-- | Reverse of a 'Clif', i.e. the reverse of all its component blades.
rev :: Ord b => Clif b a -> Clif b a
rev = Clif . rev' . unClif

-- | Reverses each blade (key)
rev' :: Ord b => Map [b] a -> Map [b] a
rev' = M.mapKeys reverse

-- | Returns the canonical form of a 'Clif'
canon :: (Eq a, Basis b a) => Clif b a -> Clif b a
canon = Clif . canon' . unClif

-- | Returns the canonical representation of a 'Clif' (blades simplified and in canonical order)
canon' :: (Eq a, Basis b a) => Map [b] a -> Map [b] a
canon' = M.filter (/=0) . M.fromListWith (+) . concatMap canonical . M.assocs

-- | Grade projection on the given grade. For negative values, returns zero.
-- 
-- Note that this always calculates the canonical form of a Clif before projecting it.
grade :: (Eq a, Basis b a) => Int -> Clif b a -> Clif b a
grade k = Clif . grade' k . unClif

-- | Filter blades (keys) by their length.
grade' :: (Eq a, Basis b a) => Int -> Map [b] a -> Map [b] a
grade' k = M.filterWithKey f . canon'
    where f = const . (==) k . length

-- | General product or contraction of Clifs using a given grade function. 
-- Given a function @f :: 'Int' -> 'Int' -> 'Int'@ and p, q-grade Clifs A and B, contractWith returns
-- the f(p,q)-grade projection of the 'Clif' A times B.
contractWith :: (Eq a, Basis b a) => (Int -> Int -> Int) -> Clif b a -> Clif b a -> Clif b a
contractWith f = sum .: liftA2 g `on` grades
    where g (r, x) (s, y) = let k = f r s in 
                               case compare k 0 of 
                                   LT -> 0
                                   _  -> grade k $ x * y
  
-- * Properties

-- | List of nonzero grades.  
--
-- Note that this always calculates the canonical form of a Clif before recovering the filled grades.
filledGrades :: (Eq a, Basis b a) => Clif b a -> [Int]
filledGrades = filledGrades' . unClif

-- | List of nonzero grades
--
-- Note that this always calculates the canonical form of a Clif before recovering the filled grades.
filledGrades' :: (Eq a, Basis b a) => Map [b] a -> [Int]
filledGrades' = nub . map length . M.keys . canon'

-- | Returns a list containing each non-zero grade component of a 'Clif' and it's grade as an 'Int'.
-- 
-- Note that this always calculates the canonical form of a Clif before testing any operations.
grades :: (Eq a, Basis b a) => Clif b a -> [(Int, Clif b a)]
grades v = [(k, grade k v) | k <- filledGrades v]

-- | True if the 'Clif' contains no nonzero blades of grade greater than zero.
-- 
-- Note that this always calculates the canonical form of a Clif before testing whether it is scalar.
isScalar :: (Eq a, Basis b a) => Clif b a -> Bool
isScalar v = ((0==) . maxGrade) v || isZero v 

-- | True for a zero multivector.
-- 
-- Note that this always calculates the canonical form of a Clif before testing whether it is zero.
isZero :: (Eq a, Basis b a) => Clif b a -> Bool
isZero = null . canon' . unClif

-- | The highest nonempty nonzero grade of a Clif.
--
-- Note that this always calculates the canonical form of a Clif before recovering the highest grade.
maxGrade :: (Eq a, Basis b a) => Clif b a -> Int
maxGrade v | isZero v  = 0
           | otherwise = maximum $ filledGrades v
                           

-- * Utility functions

{-# INLINE (.:) #-}
infixl 8 .:
-- |Composition of unary and binary functions, highly useful since two-parameter constructors are ubiquitous here. Redefined here to skip extra dependencies.
--
-- prop> (f .: g) x y = f (g x y)
--
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) = (.) . (.)
