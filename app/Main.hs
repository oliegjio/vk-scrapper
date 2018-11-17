{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
-- import Tag
import Request (get)
import Text.HTML.TagSoup (parseTags, Tag (TagOpen), fromTagText)
import Data.List (elemIndex)
import Data.Function ((&))
import Data.Maybe (maybe)

main :: IO ()
main = do
    response <- get "https://vk.com/sorokin_o"
    BS.writeFile "sandbox/content.html" response
    let tags = parseTags response
    let online = TagOpen "span" [("class", "pp_last_activity_text")] `elemIndex` tags & maybe 0 (+2)
    print tags
    print $ tags !! online
