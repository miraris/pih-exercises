signum' :: Int -> Int
signum' n
  | n > 0 = 1
  | n == 0 = 0
  | otherwise = -1

add' :: Int -> Int -> Int
add' = \x -> \y -> x + y

------------------------
hlen :: [xs] -> Int
hlen xs = length xs `div` 2

halve :: [a] -> ([a], [a])
halve xs = (take (hlen xs) xs, drop (hlen xs) xs)

halve' :: [a] -> ([a], [a])
halve' xs = splitAt (hlen xs) xs

------------------------
third :: [a] -> a
third xs = head $ tail $ tail xs

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:x:_) = x

------------------------
safetail :: [a] -> [a]
safetail xs =
  if null xs
    then []
    else tail xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs
