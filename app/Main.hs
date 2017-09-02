import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Monad (unless)
import System.Environment (getArgs)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

normal :: IO ()
normal = do
  print $ fib 39
  print $ fib 40
  print $ fib 41
  print $ fib 42

fork :: IO ()
fork = do
  tv <- newTVarIO 0
  _ <- fork' tv $ print $ fib 39
  _ <- fork' tv $ print $ fib 40
  _ <- fork' tv $ print $ fib 41
  _ <- fork' tv $ print $ fib 42
  atomically $ readTVar tv >>= flip unless retry . (== 4)
 where
  fork' :: TVar Int -> IO () -> IO ThreadId
  fork' tv p = forkIO $ p >> atomically (readTVar tv >>= writeTVar tv . succ)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("normal":_) -> normal
    ("fork":_) -> fork
    _ -> return ()

