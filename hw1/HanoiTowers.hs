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

-- @FIXME: Only "works" for 0 < n < 4
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 from to _ _ = [(from, to)]
hanoi4 n from to temp1 temp2 = (hanoi4 (n - 2) from temp1 temp2 to)
    ++ [(from, temp2), (from, to), (temp2, to)]
    ++ (hanoi4 (n - 2) temp1 to temp2 from)

