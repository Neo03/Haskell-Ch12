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

newtype Word' = Word' String
  deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord w = if lv < lc then Just (Word' w) else Nothing
  where lv = length $ filter (`elem` vowels) w
        lc = length w - lv
--where lv = length $ filter ((flip elem) vowels) w

data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just (tn n)
    where tn i
              |i == 0 = Zero
              |otherwise = Succ (tn (i - 1))
