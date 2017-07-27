{-# LANGUAGE OverloadedStrings #-}

module Text.Render (
    module Export,
    Render(..),
    indefinite,

    wrap,
    underline
) where

import Data.List (foldl', intersperse)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Export (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder


class Render a where
    render :: a -> Text

--
-- | Render "a" or "an" in front of a word depending on English's idea of
-- whether it's a vowel or not.
--
indefinite :: Text -> Text
indefinite text =
  let
    article = if T.head text `elem` ['A','E','I','O','U','a','e','i','o','u']
        then "an "
        else "a "
  in
    if T.null text
        then T.empty
        else T.append article text

--
-- | Often the input text represents a paragraph, but does not have any
-- internal newlines (representing word wrapping). This function takes a line
-- of text and inserts newlines to simulate such folding. It also appends a
-- trailing newline to finish the paragraph.
--
wrap :: Int -> Text -> Text
wrap margin text =
  let
    built = wrapHelper margin (T.words text)
  in
    L.toStrict (toLazyText built)

wrapHelper :: Int -> [Text] -> Builder
wrapHelper _ [] = ""
wrapHelper _ [x]  = fromText x
wrapHelper margin (x:xs) =
    snd $ foldl' (wrapLine margin) (T.length x, fromText x) xs

wrapLine :: Int -> (Int, Builder) -> Text -> (Int, Builder)
wrapLine margin (pos,builder) word =
  let
    width = T.length word
    width' = pos + width + 1
  in
    if width' > margin
        then (width , builder <> "\n" <> fromText word)
        else (width', builder <> " "  <> fromText word)


underline :: Char -> Text -> Text
underline level title =
    T.map (\_ -> level) title

