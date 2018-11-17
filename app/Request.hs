{-# LANGUAGE OverloadedStrings #-}

module Request ( get
               , getH
               , getBearer
               , post
               ) where

import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Simple ( httpBS
                           , parseRequest
                           , Request
                           , getResponseBody
                           , setRequestMethod
                           , addRequestHeader)
import Network.HTTP.Types.Header (HeaderName)

type URL = String
type Token = String
type HeaderValue = B8.ByteString

-- | Make a GET request to a given url. Return result as a string.
get :: URL -> IO B8.ByteString
get url = getResponseBody <$> (parseRequest url >>= httpBS)

-- | Make a GET request to a given URL with specified headers. Return string as a result.
getH :: [(HeaderName, HeaderValue)] -> URL -> IO B8.ByteString
getH headers url = do
  request <- parseRequest url
  let requestH = foldl addHeader request headers
  getResponseBody <$> httpBS requestH

-- | Make a GET request to a given URL and authenticate with given Bearer token. Return string as a result.
getBearer :: Token -> URL -> IO B8.ByteString
getBearer token = getH [("Authorization", B8.pack $ "Bearer " ++ token)]

-- | Make a POST request to a given URL. Return string as a result.
post :: URL -> IO B8.ByteString
post url = fmap getResponseBody $
  setRequestMethod "POST" <$> parseRequest url >>= httpBS

-- | Adds up given header to the given request. Return request with header appended.
addHeader :: Request -> (HeaderName, HeaderValue) -> Request
addHeader request (name, value) = addRequestHeader name value request
