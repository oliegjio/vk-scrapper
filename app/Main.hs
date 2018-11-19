{-# LANGUAGE OverloadedStrings #-}

module Main where

import Request (get)
import Text.HTML.TagSoup (parseTags, Tag (TagOpen))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Data.Time (getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))

offlineTag = TagOpen "span" [("class", "pp_last_activity_offline_text")]

getTime :: IO String
getTime = take 19 <$> show <$> getCurrentTime

getStatus :: String -> IO Bool
getStatus link = not <$> elem offlineTag <$> parseTags <$> get link

action :: String -> IO ()
action link = do
   status <- getStatus link 
   time <- getTime
   let message = time ++ " " ++ show status ++ "\n"
   print message
   appendFile "sandbox/status.txt" message

loop :: String -> IO ()
loop link = do
    async $ action link
    threadDelay $ 5 * 1000000
    loop link

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then exitWith $ ExitFailure 0 else return ()
    let link = args !! 0
    loop link
