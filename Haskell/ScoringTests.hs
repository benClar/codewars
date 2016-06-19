module Codewars.Kata.ScoringTests where

scoreTest :: (Integral a) => [a] -> a -> a -> a -> a
scoreTest li a b c = sum (map (\p -> scoreAnswer p a b c) li)
scoreAnswer :: (Integral a) => a -> a -> a -> a -> a
scoreAnswer p c o i 
    | p == 0 = c
    | p == 1 = o
    | p == 2 = -i
    | otherwise = error "Unrecognised input score"

