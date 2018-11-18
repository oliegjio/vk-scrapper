{-# LANGUAGE OverloadedStrings #-}

module Main where

import Request (get)
import Text.HTML.TagSoup (parseTags, Tag (TagOpen))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Data.Time (getCurrentTime)

offlineTag = TagOpen "span" [("class", "pp_last_activity_offline_text")]
vkLink = "https://vk.com/sorokin_o"

getTime :: IO String
getTime = take 19 <$> show <$> getCurrentTime 

getStatus :: IO ()
getStatus = do
    tags <- parseTags <$> get vkLink
    let isOnline = not $ offlineTag `elem` tags
    time <- getTime
    let message = time ++ " " ++ show isOnline ++ "\n"
    print message
    appendFile "sandbox/status.txt" message

loop :: IO ()
loop = do
    async getStatus
    threadDelay $ 5 * 1000000
    loop

main :: IO ()
main = loop
