{-|
Module      : Clif.Algebra
Copyright   : (c) Matti A. Eskelinen, 2016-2017
License     : MIT
Maintainer  : matti.a.eskelinen@gmail.com
Stability   : experimental
Portability : POSIX

This module provides the type 'Clif' for representing the elements of a Clifford algebra along with some standard operations.

See /The inner products of geometric algebra/ by Leo Dorst for a concise explanation of the different inner products.
-}
{-# LANGUAGE 
    Safe,
    FlexibleInstances,
    MultiParamTypeClasses
  #-}
module Clif.Algebra
    (
     -- * The @Clif@ type
      Clif

     -- * Constructing and deconstructing @Clifs@
    , blade, (*:)
    , vec
    , fromList

     -- * Geometric algebra operations
    , grade, rev

     -- * Outer product
    , wedge, (/\)

     -- * Inner products
    , (<\), (/>), (.|.), (<.>)
    , lContract, rContract, scalarProd, dot, hestenes

     -- * Hodge duality
    , hodge

     -- * Projections
    , proj

    ) where
import Clif.Basis
import Clif.Internal

infixl 8 <\, />, .|., <.>, /\
(<\), (/>), (.|.), (<.>), (/\) :: (Eq a, Basis b a) => Clif b a -> Clif b a -> Clif b a
-- | Infix synonym for 'lContract'
(<\) = lContract
-- | Infix synonym for 'rContract'
(/>) = rContract
-- | Infix synonym for 'scalarProd'
(.|.) = scalarProd
-- | Infix synonym for 'dot'
(<.>) = dot 
-- | Infix synonym for 'wedge'
(/\) = wedge

-- | Left contraction
lContract :: (Eq a, Basis b a) => Clif b a -> Clif b a -> Clif b a
lContract = contractWith (flip (-))

-- | Right contraction
rContract :: (Eq a, Basis b a) => Clif b a -> Clif b a -> Clif b a
rContract = contractWith (-)

-- | Scalar product (0-grade components of the blade products)
scalarProd :: (Eq a, Basis b a) => Clif b a -> Clif b a -> Clif b a
scalarProd = contractWith (const (const 0))

-- | Dot product
dot :: (Eq a, Basis b a) => Clif b a -> Clif b a -> Clif b a
dot = contractWith (abs .: (-))

-- | Hestenes dot product
hestenes :: (Eq a, Basis b a) => Clif b a -> Clif b a -> Clif b a
hestenes = contractWith f
    where f 0 _ = -1 -- negative grades don't exist, so the product vanishes
          f _ 0 = -1
          f a b = abs (a - b)

-- | Wedge product
wedge :: (Eq a, Basis b a) => Clif b a -> Clif b a -> Clif b a
wedge = contractWith (+)

-- | Hodge dual of a 'Clif' in a Clifford algebra specified by a given pseudoscalar (volume element):
--
-- prop> hodge (E <$> "abc") $ blade [E 'b'] 1 == blade (E <$> "ac") 1 
--
hodge :: (Eq a, Basis b a) => [b] -> Clif b a -> Clif b a
hodge bs = flip (*) (sgnm * i)
    where 
        i = blade bs 1   -- pseudoscalar times the sign of the metric
        sgnm = i * rev i -- sign of the metric calculated from the unit pseudoscalar

-- | Projection of Clif x in the direction of Clif y, defined as
--
-- prop>proj x y == (x <\ recip y) <\ y
--
proj :: (Eq a, Basis b a, Fractional a) => Clif b a -> Clif b a -> Clif b a
proj x y = (x `lContract` recip y) * y
