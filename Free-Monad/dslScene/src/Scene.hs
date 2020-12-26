{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments #-}

module Scene
    ( runProgram
    , Scene
    , ball
    , group
    , cube
    , def
    , CubeSizes(..)
    , BallRadius(..)
    ) where

import Control.Monad (liftM, ap)

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

data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap = liftM

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  Pure mx >>= f = f mx
  Free mx >>= f = Free $ fmap (>>= f) mx

type Scene = Free Object

liftF :: Functor f => f a -> Free f a
liftF s = Free $ fmap Pure s

ball :: Position -> BallRadius -> Scene ()
ball p r = liftF $ Ball p r ()

cube :: Position -> CubeSizes -> Scene ()
cube p r = liftF $ Cube p r ()

group' :: Scene () -> [Object ()]
group' (Pure ()) = []
group' (Free (Ball s r next)) = (Ball s r ()) : group' next
group' (Free (Cube s r next)) = (Cube s r ()) : group' next
group' (Free (Group xs next)) = (Group xs ()) : group' next

group :: Scene () -> Scene ()
group s = liftF $ Group (group' s) ()

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

