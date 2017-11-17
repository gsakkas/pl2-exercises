module Main where

import Test.QuickCheck
import Control.Monad
import Data.Ratio

-- Exercise 2

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

-- Exercise 3

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

prop_mapTree2 :: Int -> (Int -> Int) -> Tree Int -> Property
prop_mapTree2 n f t = inTree n t ==> inTree n' t'
    where n' = f n
          t' = mapTree f t


-- prop_mapTree2 :: (Int -> Int) -> Tree Int -> Bool
-- prop_mapTree2 f t = inTree n' t'
--     where n = head $ leaves t
--           n' = f n
--           t' = mapTree f t

prop_nodes_leaves :: (Int -> Int) -> Tree Int -> Bool
prop_nodes_leaves f t = (map f . nodes) t == (nodes . mapTree f) t &&
                        (map f . leaves) t == (leaves . mapTree f) t


bird :: Tree Rational
bird = T 1 [mapTree (\x -> 1 / x) (mapTree (+1) bird),
            mapTree (+1) (mapTree (\x -> 1 / x) bird)]

genPaths :: Gen [Int]
genPaths = sized $ \x -> if x > 0
                            then vectorOf x $ choose (0,1)
                            else return [0]

prop_bird_path :: Tree Rational -> Property
prop_bird_path t = forAll genPaths $ bird_path
                    where bird_path xs = path xs t == path xs trimmed_t
                            where trimmed_t = trimTree (length xs + 1) t

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

fibs :: [Integer]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

prop_fib_in_bird :: Tree Rational -> Property
prop_fib_in_bird t = forAll (sized $
    (\x -> if x > 0 then return (x * x) else return 4)) $
    (\n -> bird_fibs n t [] == take (fromIntegral n) fibs)
    where bird_fibs 1 (T y _) acc = reverse ((denominator y):acc)
          bird_fibs n (T y ts) acc =
            bird_fibs (n-1) (head ts) ((denominator y):acc)

findBird :: Rational -> Tree Rational -> Bool
findBird q t = findBird' q t 1
    where findBird' q' t' n =
            if q' `elem` (leaves $ trimTree n t')
                then True
                else findBird' q' t' (n+1)

genRationals :: Integer -> Gen Rational
genRationals seed | seed > 0 = liftM2 (%) (choose (1, seed)) (choose (1, seed))
                  | otherwise = liftM2 (%) (choose (1, 23)) (choose (1, 23))

prop_all_rationals :: Integer -> Tree Rational -> Property
prop_all_rationals seed t = forAll (genRationals seed) $ \q -> findBird q t

main = do t <- verboseCheck (prop_all_rationals 13 bird)
          print t
