module TitleCase (titleCase) where
import Data.Char 

capitalise :: String -> [String] -> String
capitalise w exceptions 
    | toLowerString w `elem` (map toLowerString exceptions) = toLowerString w
capitalise w _ = titleCaseWord w
        
titleCaseWord :: String -> String
titleCaseWord (x:xs) = toUpper x : toLowerString xs

titleCase :: String -> String -> String
titleCase minor title = unwords $ capitaliseAll (firstWord (words title)) (words minor)

capitaliseAll :: [String] -> [String] -> [String]
capitaliseAll (x:xs) exceptions = x : map (\x -> capitalise x exceptions) xs
capitaliseAll title exceptions = title

firstWord :: [String] -> [String]
firstWord (x:xs) = titleCaseWord x : xs
firstWord w = w

toLowerString :: String -> String
toLowerString s = map toLower s