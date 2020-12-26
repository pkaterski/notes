{-# LANGUAGE BlockArguments, FlexibleContexts #-}
module Main where

import  Scene4

import Control.Monad.Free (MonadFree)
import Control.Monad.Free.Church (improve)

test :: MonadFree Object m => m ()
test = do
    x <- ask
    ball def def x
    cube def $ CubeSizes 1.0 1.0 1.0
    group do
        ball def def "neno"
        group do
            cube def def
            cube def def
            cube def def
            cube def def
            y <- ask
            ball def def y
        group do
            cube def def
        ball def def x

shitTest :: MonadFree Object m => Int -> m ()
shitTest 0         = pure ()
shitTest i | i < 0 = pure ()
shitTest i = do
    x <- ask
    ball def def x
    cube def $ CubeSizes 1.0 1.0 1.0
    shitTest $ i - 1
    group do
        ball def def "neno"
        group do
            cube def def
            shitTest $ i - 1
            cube def def
            cube def def
            y <- ask
            ball def def y
        group do
            cube def def
        ball def def x
    shitTest $ i - 3

        

main :: IO ()
-- main = runProgram test
main = runProgram
        $ improve
        $ shitTest 2
