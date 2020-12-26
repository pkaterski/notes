{-# LANGUAGE ViewPatterns #-}

import Prelude hiding (pred)

main :: IO ()

pred :: Int -> Maybe Int
pred n = if n <= 0 then Nothing else Just (n - 1)

add :: Int -> Int -> Int
add (pred -> Nothing) m = m
add (pred -> Just n) m = succ (add n m)


main = do
  putStrLn "Hello"
