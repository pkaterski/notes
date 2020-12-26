{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf  #-}
import Control.Monad.RWS.Lazy
import Control.Monad.Except

type Program a = RWST String [String] Int (Except (Int, String)) a

p :: Program (Int, Int)
p = do
  conf <- ask
  tell ["I got a conf: " <> conf]
  state <- get
  tell ["The state that I got is: " <> show state]
  if | state == 1 -> throwError (-1, "The state shouldn't be 1")
     | state == 2 -> throwError (2,  "The state shouldn't be 2")
     | otherwise -> do
        modify (+1)
        state' <- get
        tell ["The new state is: " <> show state']
        rec x <- pure (state, fst y)
            y <- pure (state, fst x + 1)
        pure y

runProgram :: Show a => Program a -> IO ()
runProgram p =
  case runExcept $ runRWST p "conf file" 10 of
    Left (i, err) -> putStrLn $ "ERROR NO " <> show i <> ": " <> err
    Right (val, _state, log) -> do
      putStrLn "LOG"
      foldM_ (\_ x -> putStrLn x) () log
      putStrLn "-----------------------------"
      putStrLn $ "The result is " <> show val
  

main :: IO ()
main = runProgram p

