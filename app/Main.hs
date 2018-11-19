{-# LANGUAGE OverloadedStrings #-}

module Main where

import Request (get)
import Text.HTML.TagSoup (parseTags, Tag (TagOpen))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Data.Time (getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (..))

offlineTag = TagOpen "span" [("class", "pp_last_activity_offline_text")]

getTime :: IO String
getTime = take 19 <$> show <$> getCurrentTime

getStatus :: String -> IO ()
getStatus link = do
    tags <- parseTags <$> get link
    let isOnline = not $ offlineTag `elem` tags
    time <- getTime
    let message = time ++ " " ++ show isOnline ++ "\n"
    print message
    appendFile "sandbox/status.txt" message

loop :: String -> IO ()
loop link = do
    async $ getStatus link
    threadDelay $ 5 * 1000000
    loop link

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then exitWith $ ExitFailure 0 else return ()
    let link = args !! 0
    loop link
