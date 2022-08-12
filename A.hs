{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module A where

import Prelude

-- Function 1: Return a list of all prime numbers up to a given limit:

primes :: Int -> [Int]
primes n = [ i | i <- [1..n] , i > 1 && not (any ((== 0) . (i `mod`)) [2 .. i - 1]) ]
-- Number i is prine if it is greater than 1 and not any remained is 0 with numbers from 2 to i-1


-- Function 2: Encrypt text using Caesar’s cipher

indexOf :: Char -> [(Int,Char)] -> Int
indexOf ch [] = error "Element not Found"
indexOf c  ((idx, chr) : xs) = if c == chr then idx else indexOf c xs

elemByIndex :: Int -> [(Int,Char)] -> Char
elemByIndex n [] = error "Index out of Bound"
elemByIndex n  ((idx, chr) : xs) = if n == idx then chr else elemByIndex n xs

caesarcipher :: [Char] -> Int -> [Char]
caesarcipher str n = do
    let mapped = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 "
    let pairs = zip [0..] mapped
    map (\c -> elemByIndex (mod (n + indexOf c pairs) (length mapped)) pairs) str

-- Function 3: Implement the factorial function

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Function 4: Merge two sorted lists

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) = if x < y then x : merge xs (y : ys) else y : merge (x : xs) ys