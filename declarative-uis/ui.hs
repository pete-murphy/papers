module UI where

import Data.IORef
import Control.Monad

ui :: IORef Int -> IO ()
ui ref = do
  counter <- readIORef ref
  putStrLn (show counter)
  _ <- getLine
  writeIORef ref (counter + 1)

main :: IO ()
main = do
  ref <- newIORef 0
  forever (ui ref)
