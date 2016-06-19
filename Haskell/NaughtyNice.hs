module NaughtyNice where

type Warrior = (String, Bool)

getNiceNames :: [Warrior] -> [String]
getNiceNames    xs = map fst $ filter (\x -> check x True) xs

getNaughtyNames :: [Warrior] -> [String]
getNaughtyNames xs = map fst $ filter (\x -> check x False) xs

check :: Warrior -> Bool -> Bool
check w cond = snd w == cond
