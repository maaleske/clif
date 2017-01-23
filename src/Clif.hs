{-|
Module      : Clif
Copyright   : (c) Matti A. Eskelinen 2016-2017
License     : MIT
Maintainer  : matti.a.eskelinen@gmail.com 
Stability   : experimental
Portability : POSIX

This helper module exports the main modules needed to construct and operate on Clifford algebra values. If you just want to use the library for computations, this should be all you need. To get started, read "Clif.Tutorial".

-}
module Clif
    (
     -- * Construction of a basis
      module Clif.Basis
     -- * Algebraic operations
    , module Clif.Algebra
    
    ) where
import Clif.Basis
import Clif.Algebra
