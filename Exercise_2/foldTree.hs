module Main where

data Tree a = T a [Tree a] deriving (Show)

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

countTree :: (a -> Bool) -> Tree a -> Integer
countTree f = foldTree f'
    where f' x bs
            | f x == True = 1 + foldr (+) 0 bs
            | otherwise = foldr (+) 0 bs

leaves :: Tree a -> [a]
leaves = foldTree f
    where f x [] = [x]
          f _ bs = foldl (++) [] bs

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree f'
    where f' x bs = T (f x) bs

trimTree :: Int -> Tree a -> Tree a
trimTree 1 (T x _) = (T x [])
trimTree n (T x ts) = T x (map (trimTree (n-1)) ts)

path :: [Int] -> Tree a -> a
path [] (T y _) = y
path (x:xs) (T _ ts) = path xs (findXth x ts)
    where findXth x _ | x < 0 = error "Negative child in path!"
          findXth _ xs | null xs = error "Empty children list!"
          findXth 0 (x:_) = x
          findXth x (_:xs) = findXth (x-1) xs

main = do
--      10
--    / | \
--   8  6  4
--  / \    |
-- 3   2   1
--         |
--         0
        let t = T 10 [T 8 [T 3 [], T 2 []], T 6 [], T 4 [T 1 [T 0 []]]]
        putStrLn "An example tree for all the tests:"
        print t
        putStrLn "\nSize of Tree (sizeTree):"
        print $ sizeTree t
        putStrLn "\nHeight of Tree (heightTree):"
        print $ heightTree t
        putStrLn "\nSum of all nodes (sumTree):"
        print $ sumTree t
        putStrLn "\nMaximum value of all nodes (maxTree):"
        print $ maxTree t
        putStrLn "\nChecks if a value (e.g. 2 and 7) is in the Tree (inTree):"
        putStrLn $ show (inTree 2 t) ++ ", " ++ show (inTree 7 t)
        putStrLn "\nA list with all node values (nodes):"
        print $ nodes t
        putStrLn "\nNumber of all nodes that satisfy a predicate f (e.g. x > 4) (countTree):"
        print $ countTree (>4) t
        putStrLn "\nA list with all leaf values (leaves):"
        print $ leaves t
        putStrLn "\nThe tree that is produced if we apply f (e.g. f x = x + 42) to each node (mapTree):"
        print $ mapTree (+42) t
        putStrLn "\nThe tree that is left if we \"trim\" it at height n (e.g. 2) (trimTree):"
        print $ trimTree 2 t
        putStrLn "\nThe value of the node following the path given (e.g. [0, 1]) (path):"
        print $ path [0, 1] t
