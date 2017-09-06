import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.Async.Lifted (mapConcurrently)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Monad (unless)
import Control.Parallel.Strategies (parMap, rpar)
import System.Environment (getArgs)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

once :: IO ()
once = print $ fib 42

fourTimes1 :: IO ()
fourTimes1 = do
  print $ fib 42
  print $ fib 42
  print $ fib 42
  print $ fib 42

fourTimes2 :: IO ()
fourTimes2 = mapM_ (print . fib) [42, 42, 42, 42]

fourTimes3 :: IO ()
fourTimes3 = mapM_ (\_ -> print $ fib 42) [(), (), (), ()]

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

async :: IO ()
async = mapConcurrently_ (print . fib) [39, 40, 41, 42]

lasync :: IO ()
lasync = mapConcurrently (\a -> return $ fib a) [39, 40, 41, 42] >>= mapM_ print

eval :: IO ()
eval = do
  let as = parMap rpar fib [39, 40, 41, 42]
  mapM_ print as

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("once":_) -> once
    ("fourtimes1":_) -> fourTimes1
    ("fourtimes2":_) -> fourTimes2
    ("fourtimes3":_) -> fourTimes3
    ("normal":_) -> normal
    ("fork":_) -> fork
    ("async":_) -> async
    ("lasync":_) -> lasync
    ("eval":_) -> eval
    _ -> return ()

