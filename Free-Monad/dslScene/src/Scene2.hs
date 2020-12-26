{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments #-}

module Scene2
    ( runProgram
    , Scene
    , ball
    , group
    , cube
    , def
    , CubeSizes(..)
    , BallRadius(..)
    ) where

import Control.Monad.Free.Church (F(..), liftF, runF)

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

instance Functor Object where
    fmap f (Ball p r a) = Ball p r $ f a
    fmap f (Cube p r a) = Cube p r $ f a
    fmap f (Group xs x) = Group xs $ f x

type Scene = F Object

ball :: Position -> BallRadius -> Scene ()
ball p r = liftF $ Ball p r ()

cube :: Position -> CubeSizes -> Scene ()
cube p r = liftF $ Cube p r ()

groupPure :: () -> [Object ()]
groupPure () = []

groupImpure :: Object [Object ()] -> [Object ()]
groupImpure (Ball s r next) = (Ball s r ()) : next
groupImpure (Cube s r next) = (Cube s r ()) : next
groupImpure (Group xs next) = (Group xs ()) : next

group :: Scene () -> Scene ()
group s = do
    let g = runF s groupPure groupImpure
    liftF $ Group g ()

runProgramImpure :: Int -> Object (IO ()) -> IO ()
runProgramImpure indent (Ball p r next) = do
    printLnI indent $ "<Ball" <> posToAttr p <> sizeToAttr (BallS r) <> "></Ball>"
    next
runProgramImpure indent (Cube p r next) = do
    printLnI indent $ "<Cube" <> posToAttr p <> sizeToAttr (CubeS r) <> "></Cube>"
    next
runProgramImpure indent (Group xs next) = do
    printLnI indent "<Group>"
    mapM_ (runProgramImpure (indent + 1) . fmap pure) xs
    printLnI indent "</Group>"
    next

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
    runF program (const $ pure ()) $ runProgramImpure 1
    putStrLn "</Scene>"

-- test :: IO ()
-- test = runProgram $ shitScene
