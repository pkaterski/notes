{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments, DeriveFunctor, FlexibleContexts #-}

module Scene4
    ( runProgram
    , Scene
    , ball
    , group
    , cube
    , def
    , ask
    , CubeSizes(..)
    , BallRadius(..)
    , Object(..)
    ) where

import Control.Monad.Free (Free(..), MonadFree, wrap)

data Position =
    Position
        { pos_x :: Float
        , pos_y :: Float
        , pos_z :: Float
        }
        deriving Show

class Default a where
    def :: a

instance Default Position where
    def = Position 0.0 0.0 0.0

data CubeSizes
    = CubeSizes { cube_x :: Float, cube_y :: Float, cube_z :: Float }

data BallRadius
    = BallRadius Float

data Size
    = CubeS CubeSizes
    | BallS BallRadius

instance Default CubeSizes where
    def = CubeSizes 1.0 1.0 1.0

instance Default BallRadius where
    def = BallRadius 1.0

data Object a
    = Ball Position BallRadius String a
    | Cube Position CubeSizes a
    | Group [Object ()] a
    | Ask (String -> a)
    deriving (Functor)


type Scene = Free Object

ball :: MonadFree Object m => Position -> BallRadius -> String -> m ()
ball p r n = wrap $ Ball p r n (pure ())

cube :: MonadFree Object m => Position -> CubeSizes -> m ()
cube p r = wrap $ Cube p r (pure ())

ask :: MonadFree Object m => m String
ask = wrap $ Ask pure

group' :: String -> Scene () -> [Object ()]
group' _ (Pure ()) = []
group' i (Free (Ball s r name next)) = (Ball s r name ()) : group' i next
group' i (Free (Cube s r next)) = (Cube s r ()) : group' i next
group' i (Free (Group xs next)) = (Group xs ()) : group' i next
group' i (Free (Ask f)) = group' i $ f i

group :: MonadFree Object m => Scene () -> m ()
group s = do
    i <- ask
    wrap $ Group (group' i s) (pure ())

runProgram' :: String -> Int -> Scene () -> IO ()
runProgram' _ _ (Pure ()) = pure ()
runProgram' s indent (Free (Ball p r name next)) = do
    printLnI indent $ "<Ball" <> posToAttr p <> sizeToAttr (BallS r) <> ">"<>name<>"</Ball>"
    runProgram' s indent next
runProgram' s indent (Free (Cube p r next)) = do
    printLnI indent $ "<Cube" <> posToAttr p <> sizeToAttr (CubeS r) <> "></Cube>"
    runProgram' s indent next
runProgram' s indent (Free (Group xs next)) = do
    printLnI indent "<Group>"
    mapM_ (runIndented . objectToScene) xs
    printLnI indent "</Group>"
    runProgram' s indent next
    where
        runIndented   = runProgram' s $ indent + 1
        objectToScene = Free . fmap Pure
runProgram' s indent (Free (Ask f)) = do
    runProgram' s indent $ f s

printLnI :: Int -> String -> IO ()
printLnI indent str = do
    mapM_ putStr ["  " | _ <- [0..indent - 1]]
    putStrLn str

posToAttr :: Position -> String
posToAttr p
    =  " pos_x=\"" <> (show $ pos_x p) <> "\""
    <> " pos_y=\"" <> (show $ pos_y p) <> "\""
    <> " pos_z=\"" <> (show $ pos_z p) <> "\""

sizeToAttr :: Size -> String
sizeToAttr (CubeS p)
    =  " side_x=\"" <> (show $ cube_x p) <> "\""
    <> " side_y=\"" <> (show $ cube_y p) <> "\""
    <> " side_z=\"" <> (show $ cube_z p) <> "\""
sizeToAttr (BallS (BallRadius r))
    =   " radius=\"" <> (show $ r) <> "\""

runProgram :: Scene () -> IO ()
runProgram program = do
    putStrLn "<Scene>"
    runProgram' "input" 1 program
    putStrLn "</Scene>"

