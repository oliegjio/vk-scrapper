module Tag ( tagName
           , tagsWithName
           ) where

import Text.HTML.TagSoup

tagName :: Tag s -> Maybe s
tagName (TagOpen n _) = Just n
tagName (TagClose n) = Just n
tagName _ = Nothing

tagsWithName :: Eq s => s -> [Tag s] -> [Tag s]
tagsWithName n = filter (\t -> tagName t == Just n)
