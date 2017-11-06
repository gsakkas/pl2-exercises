{-# OPTIONS_GHC -O2 -optc-O2 #-}

module Main where

import Data.Char (isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show) 
data Direction a = LeftD a (Tree a) | RightD a (Tree a) deriving (Show) 
type Directions a = [Direction a]
type Zipper a = (Tree a, Directions a)

goLeft :: Zipper Int -> Maybe (Zipper Int)
goLeft (Empty, _) = Nothing
goLeft (Node x l r, ds) = Just (l, (LeftD x r):ds)

goRight :: Zipper Int -> Maybe (Zipper Int)
goRight (Empty, _) = Nothing
goRight (Node x l r, ds) = Just (r, (RightD x l):ds)

goUp :: Zipper Int -> Maybe (Zipper Int)
goUp (_, []) = Nothing
goUp (t, (LeftD x r):ds) = Just (Node x t r, ds)
goUp (t, (RightD x l):ds) = Just (Node x l t, ds)

-- attach :: Tree Int -> Zipper Int -> Zipper Int
-- attach t (_, ds) = (t, ds)

topMost :: Zipper Int -> Maybe (Zipper Int)
topMost (t, []) = Just (t, [])
topMost z = return z >>= goUp >>= topMost

goRightmost :: Zipper Int -> Maybe (Zipper Int)
goRightmost (Empty, ds) = Just (Empty, ds)
goRightmost z = return z >>= goRight >>= goRightmost

goUpUntilGreater :: Tree Int -> Zipper Int -> Maybe (Zipper Int)
goUpUntilGreater (Node y tl tr) (t, []) = Just (Node y t tr, [])
goUpUntilGreater _ (t, (LeftD x r):ds) = Nothing
goUpUntilGreater node@(Node y tl tr) original@(t, (RightD x l):ds)
    | x >= y = Just (Node y t tr, (RightD x l):ds)
    | otherwise = return original >>= goUp >>= goUpUntilGreater node
                        
buildTree :: [Int] -> Zipper Int -> Zipper Int
buildTree [] zipper = zipper
buildTree (x:xs) acc =
    let newNode = Node x Empty Empty
        rightmost = return acc >>= goRightmost
        zipper = fromJust (rightmost >>= goUpUntilGreater newNode)
    in buildTree xs zipper

findSum :: Tree Int -> Int -> Int -> Int
findSum Empty _ _ = 0
findSum (Node x _ _) y m = 1 + (abs (y - x) - 1) `div` m

sound :: Zipper Int -> Int -> Int
sound (Empty, _) _ = 0
sound zipper@(Node x l r, ds) m =
  let
    (ln, lds) = fromJust $ goLeft zipper
    (rn, rds) = fromJust $ goRight zipper
    ql = findSum ln x m
    qr = findSum rn x m
  in sound (ln, lds) m + sound (rn, rds) m + ql + qr
    

main = 
  do all <- BS.getContents
     let Just (n, r1) = readInt all
     let Just (m, r2) = readInt r1
     let xs = readMany readInt r2
     let t = fromJust $ topMost $ buildTree xs (Empty, [])
     print $ sound t m
  where readInt s = BSC.readInt (BSC.dropWhile isSpace s)
        readMany readf s = case readf s of
            Just (x, r) -> let xs = readMany readf r
                           in  x:xs
            Nothing     -> []