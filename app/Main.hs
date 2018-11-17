{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
import Text.HTML.TagSoup
import Data.List (elemIndex)

main :: IO ()
main = do
    response <- httpBS "https://vk.com/sorokin_o"
    let responseBody = getResponseBody response
    let tags = parseTags responseBody
    let userName = TagOpen "h2" [("class", "page_name")] `elemIndex` tags
    print tags
    print userName
