{-# OPTIONS_GHC -Wall #-}

module HanoiTowers where

type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _    _  _    = []
hanoi 1 from to _    = [(from, to)]
hanoi n from to temp = (hanoi (n - 1) from temp to)
                       ++ [(from, to)]
                       ++ (hanoi (n - 1) temp to from)

