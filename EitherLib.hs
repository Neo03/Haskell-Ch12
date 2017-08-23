module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Right a) xs = xs
    f (Left a) xs = a : xs

rights':: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Left a) xs = xs
    f (Right a) xs = a : xs

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' = foldr f ([], [])
  where
    f (Left a)(xs, ys) = (a:xs, ys)
    f(Right b)(xs, ys) = (xs, b: ys)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> Maybe c
either' fl _ (Left a) = Just (fl a)
either' _ fr (Right b) = Just (fr b)

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f (Right b) = Just (f b)
