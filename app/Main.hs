module Main where

main :: IO ()
main = putStrLn "Hello world"
{-}
import Parser

import Control.Monad.Trans
import System.Console.Haskeline

{- 
  parses the current line of input
  either prints an error message or the ast
-}
process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> outputStr ""
      Just input -> (liftIO $ process input) >> loop
-}