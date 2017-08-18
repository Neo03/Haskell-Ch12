module Ex1 where
import Data.List(intercalate)

notThe :: String -> Maybe String
notThe s
  | s == "the" = Nothing
  | otherwise = Just s


replaceThe :: String -> String
replaceThe str = intercalate " " $ map athe $ fmap notThe $ words str
  where athe Nothing = "a"
        athe (Just x) = x

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = f 0 $ words s

isVowel :: Char -> Bool
isVowel x = x `elem` "aeoui"

f :: Integer -> [String] -> Integer
f n (x:y:ys)
  | x == "the" && isVowel (head y) = f (n+1) (y:ys)
  | otherwise = f n ys
f n _ = n

countVowels :: String -> Int
countVowels = length . wordVowels

wordVowels :: String -> String
wordVowels = filter  isVowel
