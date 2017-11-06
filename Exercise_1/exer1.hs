{-# OPTIONS_GHC -O2 -optc-O2 #-}

module Main where

import Data.Char (isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC


sound :: [Integer] -> [Integer] -> Integer -> Integer -> Integer
sound [] _ _ acc  = acc
sound (x:xs) (y:[]) m acc
      | x < y     = sound xs (x:y:[]) m (acc + q1)
      | x > y     = sound xs [x] m (acc + q2)
      | otherwise = sound xs (y:[]) m acc
          where q1 = 1 + ((abs (y - x) - 1) `div` m)
                q2 = 1 + ((abs (x - y) - 1) `div` m)
sound (x:xs) (y:ys) m acc
      | x < y     = sound xs (x:y:ys) m (acc + q)
      | x > y     = sound (x:xs) ys m acc
      | otherwise = sound xs (y:ys) m acc
          where q = 1 + ((abs (y - x) - 1) `div` m)

main = 
  do all <- BS.getContents
     let Just (n, r1) = readInteger all
     let Just (m, r2) = readInteger r1
     let (x:xs) = readMany readInteger r2
     print $ sound xs [x] m 0
  where readInteger s = BSC.readInteger (BSC.dropWhile isSpace s)
        readMany readf s = case readf s of
            Just (x, r) -> let xs = readMany readf r
                           in  x:xs
            Nothing     -> []
