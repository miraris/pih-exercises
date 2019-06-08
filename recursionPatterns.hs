-- map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

-- fold
fold' :: (a -> b -> b) -> b -> [a] -> b
fold' f z [] = z
fold' f z (x:xs) = f x $ fold' f z xs
