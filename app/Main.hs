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

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("normal":_) -> normal
    _ -> return ()

