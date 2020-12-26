{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments, DeriveFunctor, FlexibleContexts #-}

module Scene3
    ( runProgram
    , Scene
    , ball
    , group
    , cube
    , def
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
    = Ball Position BallRadius a
    | Cube Position CubeSizes a
    | Group [Object ()] a
    deriving (Functor)


type Scene = Free Object

ball :: MonadFree Object m => Position -> BallRadius -> m ()
ball p r = wrap $ Ball p r (pure ())

cube :: MonadFree Object m => Position -> CubeSizes -> m ()
cube p r = wrap $ Cube p r (pure ())

group' :: Scene () -> [Object ()]
group' (Pure ()) = []
group' (Free (Ball s r next)) = (Ball s r ()) : group' next
group' (Free (Cube s r next)) = (Cube s r ()) : group' next
group' (Free (Group xs next)) = (Group xs ()) : group' next

group :: MonadFree Object m => Scene () -> m ()
group s = wrap $ Group (group' $ s) (pure ())

runProgram' :: Int -> Scene () -> IO ()
runProgram' _ (Pure ()) = pure ()
runProgram' indent (Free (Ball p r next)) = do
    printLnI indent $ "<Ball" <> posToAttr p <> sizeToAttr (BallS r) <> "></Ball>"
    runProgram' indent next
runProgram' indent (Free (Cube p r next)) = do
    printLnI indent $ "<Cube" <> posToAttr p <> sizeToAttr (CubeS r) <> "></Cube>"
    runProgram' indent next
runProgram' indent (Free (Group xs next)) = do
    printLnI indent "<Group>"
    mapM_ (runIndented . objectToScene) xs
    printLnI indent "</Group>"
    runProgram' indent next
    where
        runIndented   = runProgram' $ indent + 1
        objectToScene = Free . fmap Pure

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
    runProgram' 1 program
    putStrLn "</Scene>"

