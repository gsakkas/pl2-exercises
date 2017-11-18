module Main where

import TreeLib -- Exercise 2
import Test.QuickCheck
import Control.Monad
import Data.Ratio

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized tree
        where tree 0 = liftM (\x -> T x []) arbitrary
              tree n = liftM2 T arbitrary kids
                    where kids = do m <- choose (0, n `div` 2)
                                    vectorOf m (tree (n `div` 2))

prop_heightTree :: Tree Int -> Bool
prop_heightTree t = (height > 0) && (height <= sizeTree t)
    where height = heightTree t

prop_maxTree :: Tree Int -> Bool
prop_maxTree t = inTree (maxTree t) t

prop_nodesInTree :: Tree Int -> Bool
prop_nodesInTree t = foldl (&&) True ts
    where ts = map (\x -> inTree x t) (nodes t)

prop_countTree :: (Int -> Bool) -> Tree Int -> Bool
prop_countTree f t = (nodes >= 0) && (nodes <= sizeTree t)
    where nodes = countTree f t

prop_sizeTree :: Tree Int -> Bool
prop_sizeTree t = (num_of_nodes == sizeTree t) && cond
    where num_of_nodes = length $ nodes t
          num_of_leaves = length $ leaves t
          cond = if num_of_nodes == 1
                    then num_of_leaves == num_of_nodes
                    else num_of_leaves < num_of_nodes

prop_mapTree :: (Int -> Int) -> Tree Int -> Bool
prop_mapTree f t = sizeTree t == sizeTree t' &&
                   heightTree t == heightTree t'
    where t' = mapTree f t

prop_mapTree2 :: (Int -> Int) -> Int -> Tree Int -> Property
prop_mapTree2 f n t = inTree n t ==> inTree n' t'
    where n' = f n
          t' = mapTree f t

prop_nodes_leaves :: (Int -> Int) -> Tree Int -> Bool
prop_nodes_leaves f t = (map f . nodes) t == (nodes . mapTree f) t &&
                        (map f . leaves) t == (leaves . mapTree f) t

-- A fucntion that create lazily the Bird Tree
-- The Bird Tree: (http://www.cs.ox.ac.uk/ralf.hinze/publications/Bird.pdf)
bird :: Tree Rational
bird = T 1 [mapTree (\x -> 1 / x) (mapTree (+1) bird),
            mapTree (+1) (mapTree (\x -> 1 / x) bird)]

-- A generator for random paths in a binary tree
genPaths :: Gen [Int]
genPaths = sized $ \x -> if x > 0
                            then vectorOf x $ choose (0,1)
                            else return [0]

prop_bird_path :: Tree Rational -> Property
prop_bird_path t = forAll genPaths $ bird_path
                    where bird_path xs = path xs t == path xs trimmed_t
                            where trimmed_t = trimTree (length xs + 1) t

-- A generator for a ZigZag path in a binary tree
genZigZagPaths :: Gen [Int]
genZigZagPaths = sized $ \x -> if x > 0
                                then return $ concat (replicate (x*x*x) [1,0])
                                else return [1,0,1,0]

prop_all_naturals :: Tree Rational -> Property
prop_all_naturals t = forAll genZigZagPaths $
    (\xs -> bird_nums xs t [] == [1..(fromIntegral $ length xs + 1)])
    where bird_nums [] (T y _) acc = reverse ((numerator y):acc)
          bird_nums (x:xs) (T y ts) acc =
            bird_nums xs (findXth x ts) ((numerator y):acc)
            where findXth x _ | x < 0 = error "Negative child in path!"
                  findXth _ xs | null xs = error "Empty children list!"
                  findXth 0 (x:_) = x
                  findXth x (_:xs) = findXth (x-1) xs

-- A simple (and infinite) implementation of the Fibonacci sequence
fibs :: [Integer]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

prop_fib_in_bird :: Tree Rational -> Property
prop_fib_in_bird t = forAll (sized $
    (\x -> if x > 0 then return (x * x) else return 4)) $
    (\n -> bird_fibs n t [] == take (fromIntegral n) fibs)
    where bird_fibs 1 (T y _) acc = reverse ((denominator y):acc)
          bird_fibs n (T y ts) acc =
            bird_fibs (n-1) (head ts) ((denominator y):acc)

-- Returns True if the Rational number given is found in the bird tree
-- Otherwise, it doesn't terminate
-- (All Rationals are in the bird tree so it should at some point...)
findBird :: Rational -> Tree Rational -> Bool
findBird q t = findBird' q t 1
    where findBird' q' t' n =
            if q' `elem` (leaves $ trimTree n t')
                then True
                else findBird' q' t' (n+1)

-- The same as "findBird", but is faster since it checks
-- only the path till the number is found
-- It exploits the way the Bird tree is defined
findBirdFast :: Rational -> Tree Rational -> Bool
findBirdFast q (T x ts) | x == q = True
                        | x < q = findBirdFast q (maxTr ts)
                        | otherwise = findBirdFast q (minTr ts)
                            where maxTr [left@(T y _), right@(T z _)] = 
                                    if y > z
                                        then left
                                        else right
                                  minTr [left@(T y _), right@(T z _)] =
                                    if y > z
                                        then right
                                        else left

-- A simple generator for small Rational numbers with an upper bound given in seed
genRationals :: Integer -> Gen Rational
genRationals seed | seed > 0 = liftM2 (%) (choose (1, seed)) (choose (1, seed))
                  | otherwise = liftM2 (%) (choose (1, 23)) (choose (1, 23))

prop_all_rationals :: Integer -> Tree Rational -> Property
prop_all_rationals seed t = forAll (genRationals seed) $ \q -> findBirdFast q t

main = do
        putStrLn "Check that the height of a tree is a positive integer and is less than the size of the Tree (prop_heightTree):"
        t <- quickCheck prop_heightTree
        print t
        putStrLn "\nCheck that the maximum value of the tree is actually in the tree (prop_maxTree):"
        t <- quickCheck prop_maxTree
        print t
        putStrLn "\nCheck that all tree nodes are actually in the tree (prop_nodesInTree):"
        t <- quickCheck prop_nodesInTree
        print t
        putStrLn "\nCheck that the number of all nodes that satisfy a predicate f (e.g. x > 4) is an integer that is less than the size of the tree (prop_countTree):"
        t <- quickCheck $ prop_countTree (>4)
        print t
        putStrLn "\nCheck that the number of the tree nodes is equal to its size"
        putStrLn "Also check that the number of its leaves is less than the size of the tree, unless both numbers are 1 (prop_sizeTree):"
        t <- quickCheck prop_sizeTree
        print t
        putStrLn "\nCheck that the function mapTree keeps the size and the height of the tree intact (prop_mapTree):"
        t <- quickCheck $ prop_mapTree (+42)
        print t
        putStrLn "\nCheck that, if n is in the original tree, then the value f n (e.g. f x = x + 42) is in the mapped tree (prop_mapTree2):"
        t <- quickCheck $ prop_mapTree2 (+42)
        print t
        putStrLn "\nCheck for functions g ∈ {\"nodes\", \"leaves\"} that \"map f . g == g. mapTree f\" (prop_nodes_leaves):"
        t <- quickCheck $ prop_nodes_leaves (+42)
        print t
        putStrLn "\n\nGiven the Bird Tree that we implemented we have the following tests (bird):"
        putStrLn "\nCheck if given a path of length n, when we follow it in \"bird\" or in \"trimTree n bird\" we get the same node (prop_bird_path):"
        t <- quickCheck $ prop_bird_path bird
        print t
        putStrLn "\nCheck that following a ZigZag path in the Bird tree gives us all the natural numbers (prop_all_naturals):"
        t <- quickCheck $ prop_all_naturals bird
        print t
        putStrLn "\nCheck that following the leftmost path in the Bird tree gives us the Fibonacci sequence (prop_fib_in_bird):"
        t <- quickCheck $ prop_fib_in_bird bird
        print t
        putStrLn "\nCheck that every Rational number appears in the Bird tree (prop_all_rationals):"
        t <- quickCheck $ prop_all_rationals 4213 bird
        print t
