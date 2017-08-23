module LibMaybe where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust       _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' xs = foldr f [] xs
  where
    f Nothing ys = ys
    f (Just a) ys = a:ys

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = foldr f (Just []) xs
  where
    f _ Nothing = Nothing
    f Nothing _ = Nothing
    f (Just a) (Just b) = Just (a:b)
