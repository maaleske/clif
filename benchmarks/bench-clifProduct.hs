{-
 - A simple benchmark for the geometric product.
-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns, RankNTypes #-}
module Main where
import Clif
import Data.List

import Data.Time.Clock

alphabet = [1 .. 20] :: [Int]
blades = subsequences alphabet

eBlades = map ((1 *:) . map E) blades :: [Clif (Euclidean Int) Int]
lBlades = map ((1 *:) . map S) blades :: [Clif (Lorentzian Int) Int]

prods bs = zipWith (*) bs bs

main :: IO ()
main = do
    putStrLn $ "Calculating the sum of " ++ show (length blades) ++ " geometric products took ..."
    start1 <- getCurrentTime
    let !s1 = sum (prods eBlades)
    end1 <- getCurrentTime
    putStrLn $ show (diffUTCTime end1 start1) ++ " with Euclidean Int basis"

    start2 <- getCurrentTime
    let !s2 = sum (prods lBlades)
    end2 <- getCurrentTime
    putStrLn $ show (diffUTCTime end2 start2)++ " with Lorentzian Int basis"
