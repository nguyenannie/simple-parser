module Main where

import Data.Char (toUpper)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import ExpParser

main :: IO ()
main = loop processCommand M.empty
    
-- where g (state, s) = (state + 1 , show state ++ "-" ++ map toUpper s)

loop :: ((s, String) -> (s, String)) -> s -> IO ()
loop g state = do
    input <- getLine
    if input /= "q" 
        then do
            let (state', output) = g (state, input)
            putStrLn output
            loop g state'
        else pure ()

parse :: (Map String String, String) -> (Map String String, String)
parse (state, input) = case words input of
    ["let", key, "=", value] -> (M.insert key value state, "done")
    ["get", key] -> (state, fromMaybe "not exists" $ M.lookup key state)
    _ -> (state ,"not valid syntax")

parse' :: String -> Maybe Command
parse' input = case words input of
    ["let", key, "=", value] -> Just $ Let key value
    ["get", key] -> Just $ Get key
    _ -> Nothing

processCommand :: (Map String String, String) -> (Map String String, String)
processCommand (state, input) = case parse' input of
    Just (Let key value) -> (M.insert key value state, "done")
    Just (Get key) -> (state, fromMaybe "not exists" $ M.lookup key state)
    Nothing -> (state ,"not valid syntax")

