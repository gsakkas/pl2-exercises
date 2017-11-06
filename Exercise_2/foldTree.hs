module Main where

data Tree a = T a [Tree a]

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (T x ts) = f x (map (foldTree f) ts)

sizeTree :: Num b => Tree a -> b
sizeTree = foldTree f
    where f x bs = 1 + foldr (+) 0 bs

heightTree :: (Ord b, Num b) => Tree a -> b
heightTree = foldTree f
    where f x [] = 1
          f x bs = 1 + maximum bs

sumTree :: Num a => Tree a -> a
sumTree = foldTree f
    where f x bs = x + foldr (+) 0 bs

maxTree :: Ord a => Tree a -> a
maxTree = foldTree f
    where f x [] = x
          f x bs = max x (maximum bs)

inTree :: Eq a => a -> Tree a -> Bool
inTree x = foldTree f
    where f y bs
            | x == y = True
            | otherwise = foldr (||) False bs

nodes :: Tree a -> [a]
nodes = foldTree f
    where f x bs = foldl (++) [x] bs

main =
    let t = T 10 [T 8 [T 3 [], T 2 []], T 6 [], T 4 [T 1 [T 0 []]]]
--      10
--    / | \
--   8  6  4
--  / \    |
-- 3   2   1
--         |
--         0
    in print $ nodes t