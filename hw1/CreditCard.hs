{-# OPTIONS_GHC -Wall #-}

module CreditCard where

import Data.Char (digitToInt)

toDigits, toDigitsRev :: Integer -> [Integer]
toDigits x = if x <= 0 then []
             else map (toInteger . digitToInt) (show x)
toDigitsRev = reverse . toDigits

-- @TODO: This _must_ be more elegant
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . aux . reverse where
    aux []     = []
    aux [x]    = [x]
    aux (x:xs) = x : auxAlt xs
    -- Empty list case not needed since [x] above takes care of that;
    -- not sure if it should be handled anyway since GHC complains
    auxAlt [x]    = [2 * x]
    auxAlt (x:xs) = (2 * x) : aux xs

-- @TODO: Could be better?
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

