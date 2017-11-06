{-# OPTIONS_GHC -O2 -optc-O2 #-}

module Main where

import Data.Char (isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC


splitAtFirst :: Int -> [Int] -> ([Int], [Int])
splitAtFirst x = fmap (drop 1) . break (x ==)

sound :: [Int] -> Int -> (Int, Maybe Int)
sound [] _ = (0, Nothing)
sound [x] _ = (0, Just x)
sound xs m =
  let
    mx = maximum xs
    (left, right) = splitAtFirst mx xs
    (l, lmx) = sound left m
    (r, rmx) = sound right m
    ql = maybe 0 (\x -> 1 + ((abs (mx - x) - 1) `div` m)) lmx
    qr = maybe 0 (\x -> 1 + ((abs (mx - x) - 1) `div` m)) rmx
  in
    (l + r + ql + qr, Just mx)

main = 
  do all <- BS.getContents
     let Just (n, r1) = readInt all
     let Just (m, r2) = readInt r1
     let xs = readMany readInt r2
     let (res, _) = sound xs m
     print res
  where readInt s = BSC.readInt (BSC.dropWhile isSpace s)
        readMany readf s = case readf s of
            Just (x, r) -> let xs = readMany readf r
                           in  x:xs
            Nothing     -> []
