import Prelude hiding (log)
import Control.Monad (liftM, ap)

data Free f a
  = Pure a
  | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap = liftM

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  Pure mx >>= f = f mx 
  Impure mx >>= f = Impure $ fmap (>>= f) mx 


data Cmd f
  = Log String f
  | Ask (Int -> f) 

instance Functor Cmd where
  fmap g (Log s f) = Log s (g f) 
  fmap g (Ask f) = Ask $ g . f 

type Program a = Free Cmd a

log :: String -> Program ()
log s = Impure $ Log s $ Pure ()

ask :: Program Int
ask = Impure $ Ask $ \x -> Pure x


askAndLog :: Program ()
askAndLog = do
  log "waiting for user input"
  x <- ask
  log "got user input"
  log "showing user input"
  log $ show x
  log "DONE"


runProgramPure :: Int -> Program a -> ([String],a)
runProgramPure _ (Pure x) = ([], x)
runProgramPure n (Impure (Log str k)) =
  let (rest, result) = runProgramPure n k
  in (str:rest, result) 
runProgramPure n (Impure (Ask f)) = runProgramPure n $ f n

runProgramIO :: Program a -> IO a
runProgramIO (Pure x) = pure x
runProgramIO (Impure (Log str k)) = do
  putStrLn str
  runProgramIO k
runProgramIO (Impure (Ask f)) = do
  n <- getLine 
  runProgramIO $ f $ read n
  

main = undefined
