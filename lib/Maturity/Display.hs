{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Maturity.Display
(
    display,
    describe
) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Render (wrap, underline)

import Maturity.Types
import Maturity.Component
import Maturity.Instances ()

--
-- | Display descriptive meta information about a Rubric type.
--
{-
    The scoped type annotation on `rubric` is necessary to bring type a into
    scope at the term level (so we can then use TypeApplications). Go figure.
-}
display :: (Rubric a, Enum a, Bounded a) => a -> Text
display (_ :: a) =
  let
    maxScore =
        T.pack (show (fromEnum (maxBound @a)))
    value =
        T.concat ["(", maxScore, " points)"]
    levels =
        enumFrom (minBound @a)
    descriptions = 
        fmap (displayLevel) levels
  in
    T.concat
        [ title @a
        , " "
        , value
        , "\n"
        , underline '-' (title @a)
        , "\n\n"
        , T.intercalate "\n\n" descriptions
        , "\n"
        ]

--
-- | Given a Rubric value, output a wrapped version of its description as if it
-- were a numbered (<ol>, in HTML speak) list, with the remainder of the
-- description's lines offset to the right by a hanging indent.
--
displayLevel :: (Rubric a, Enum a, Bounded a) => a -> Text
displayLevel level =
  let
    paragraph = wrap 55 (description level)     -- WARNING MAGIC NUMBER
    number = fromEnum level
    list = T.lines paragraph
  in
    hangingIndent number list

hangingIndent :: Int -> [Text] -> Text
hangingIndent _ [] =
    T.empty
hangingIndent ordinal [first] =
    listItem ordinal first
hangingIndent ordinal (first:remainders) =
  let
    first' = listItem ordinal first
    remainders' = fmap (T.append "   ") remainders
  in
    T.intercalate "\n" (first':remainders')

listItem :: Int -> Text -> Text
listItem ordinal first =
    T.concat
        [ T.pack (show ordinal)
        , ". "
        , first
        ]


describe :: Model -> Text
describe
    (Model
        (Technical conceptual technical)
        (Operational customer security service)
    ) =
    T.intercalate "\n"
        [ describeMaturity "Technical Maturity"
        , display conceptual
        , display technical
        , ""
        , describeMaturity "Operational Maturity"
        , display customer
        , display security
        , display service
        ]

describeMaturity :: Text -> Text
describeMaturity headline =
    T.concat
        [ headline
        , " (10 points total)"
        , "\n"
        , underline '=' headline
        , "\n"
        ]

