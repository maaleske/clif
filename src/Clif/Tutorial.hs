{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-| 
Module      : Clif.Tutorial
Copyright   : (c) Matti A. Eskelinen, 2016-2017
License     : OtherLicense

/Clifford algebras/ (or /geometric algebras/) are itself mathematically interesting objects, but also a useful tool for vector algebra. This library attempts to make the Clifford algebraic computations easy and at least somewhat computationally efficient, while keeping the implementation as general as possible. 

Since definitions and terminology vary greatly, here is a (non-rigorous) summary of terms that will be used in this tutorial:

 - A /vector/ is just an element of some set (or type). Note that this set may in principle be infinite.
 - A /basis/ is a set of vectors along with a bilinear form which maps a pair of them to some number (see "Clif.Basis").
 - A /blade/ is any finite concatenation (/free product/) of vectors which contains each vector at most once, possibly multiplied by a scalar.
 - A /scalar/ is just a number, or a number multiplying the /empty product/.
 - A 'Clif' is a collection (direct sum) of the empty product, distinct blades and their multipliers (often called a /multivector/).
 - The /Clifford algebra/ is the set (type) of 'Clif's with the Clifford (geometric) product and direct summation.
 
cf. <https://en.wikipedia.org/wiki/Clifford_algebra>
 
 -}
module Clif.Tutorial 
    (   
     -- * Getting started
     -- $intro

     -- ** Defining our algebra
     -- $basis

     -- ** Construction of 'Clif' values
     -- $construction

     -- * Computation
     -- $computation
)
where
import Clif

{- $intro
    To begin, we just need to import the main module of the library, "Clif".

> import Clif

    This provides us with

     - Constructors for the type 'Clif' with 'Num', 'Eq' and other instances that implement the Clifford algebra,
     - Constructors for 'Euclidean' and 'Lorentzian' basis vectors,
     - The typeclass 'Basis' for constructing our own algebras,
     - Operations of the Clifford algebra defined in "Clif.Algebra".

    
-}

{- $basis
    The type @'Clif' b a@ joins the type @b@ (for /basis/) and some field (or ring) @'Num' a@ together to form a Clifford algebra. Only 'Clif's with matching types @b@ and @a@ can be multiplied directly. To generate the Clifford product between any types @b@ and @a@, we need to specify the bilinear form between them. This is done by providing an instance of the type class @'Basis' b a@. Let us do that for @'Basis' 'Char' Double@:

>instance Basis Char Double where
>    metric 't' 't' = -1
>    metric  a   b  = if a == b then 1 else 0

    The minimal complete definition for 'Basis' is the function 'metric', which we have here defined to be a diagonal quadratic form on 'Char'. This is all we need to define the reasonably high-dimensional Clifford algebra __Cl__(1,n)(R) where n is the number of Unicode codepoints represented by 'Char', with the signature (-, +, +, ...) for (\'t\', \'a\', \'b\', ...). 

    Few notes:

     - We could have used the provided instance for the newtype 'Lorentzian' to wrap 'Char' with a similar metric:

>instance (Ord b, Num a) => Basis (Lorentzian b) a where
>   metric (T a) (T b) = if a == b then -1 else 0
>   metric (S a) (S b) = if a == b then  1 else 0
>   metric _ _         = 0

       In that case, @'T' a@ for any @'Char' a@ would have the same signature as the character \'t\' in our definition. However, defining a metric for the plain 'Char' is useful for demonstration, since it provides us with pretty printouts.
     
     - Note that as in the definition for @'Basis' ('Lorentzian' b) a@, we do not actually need to fix the field @ə@ apart from the 'Num' constraint while defining the basis. 

     - If the metric is not diagonal (@'metric' a b /= 0@ for some @a /= b@), we need to replace the default implementations of the functions 'canonical' and 'basisMul' in the instance with more general implementations. See "Clif.Basis" for details.
     
-}


{- $construction

    Using our 'Char' basis, we can start constructing values of type @'Clif' 'Char' 'Double'@ using the provided constructors. We can start by introducing the vectors needed to represent __Cl__(1,3)(R):

> t = vec 't' 1
> x = vec 'x' 1
> y = vec 'y' 1
> z = vec 'z' 1

    To make it specific that we are working in __Cl__(1,3)(R), we can define the /pseudoscalar/ @txyz@. All the following definitions are equivalent:

> i = t * x * y * z

> i = blade "txyz" 1

> i = 1 *: "txyz"
    
    Note that the last form using the infix operator '(*:)' is used for the 'Show' instance to produce concise output.
    p
    Due to the 'Num' instance, we do not usually need to explicitly embed scalars. If we want to be specific, we can write

> answer = 42 :: Clif Char Double

-}

{- $computation

    We can now use any of the available operations to calculate on the 'Clif' values, such as

    - Simple multivector algebra:

>>> 2 * x * y - y * x
1.0 *: "xy" 

    - Wedge products:

>>> x /\ y
1.0 *: "xy" 

    - Reversion:

>>> rev i
1.0 *: "zyxt"
    
    - Since 'Double's have inverses, so do 'Clif's for which @v * v@ is scalar. Trivial example is vectors:

>>> x * y * z / y
-1.0 *: "xy"

-}
