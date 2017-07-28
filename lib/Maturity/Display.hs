{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Maturity.Display
(
    describeLevel,
    describeModel,
    outputScores
) where

import Data.List (foldl')
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
describeLevel :: (Rubric a, Enum a, Bounded a) => a -> Text
describeLevel (_ :: a) =
  let
    maxScore =
        T.pack (show (fromEnum (maxBound @a)))
    value =
        T.concat ["(", maxScore, " points)"]
    levels =
        enumFrom (minBound @a)
    descriptions = 
        fmap (formatLevel) levels
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
formatLevel :: (Rubric a, Enum a, Bounded a) => a -> Text
formatLevel level =
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


describeModel :: Model -> Text
describeModel
    (Model
        (Technical conceptual technical)
        (Operational customer security service)
    ) =
    T.intercalate "\n"
        [ describeMaturity "Technical Maturity"
        , describeLevel conceptual
        , describeLevel technical
        , ""
        , describeMaturity "Operational Maturity"
        , describeLevel customer
        , describeLevel security
        , describeLevel service
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


outputScores :: Model -> Text
outputScores
    (Model
        (Technical conceptual technical)
        (Operational customer security service)
    ) =
  let
  in
    T.concat
        [ formatScores
            [valuesFrom conceptual, valuesFrom technical, Nothing]
        , " Technical Maturity"
        , "\n"
        , formatScores
            [valuesFrom customer, valuesFrom security, valuesFrom service]
        , " Operational Maturity"
        ]

valuesFrom :: (Rubric a, Enum a, Bounded a) => a -> Maybe (Int,Int)
valuesFrom (rubric :: a) =
  let
    score = fromEnum rubric
    maxval = fromEnum (maxBound @a)
  in
    Just (score,maxval)


formatScores
    :: [Maybe (Int,Int)]
    -> Text
formatScores scores =
    foldl' represent T.empty scores
  where
    represent :: Text -> Maybe (Int,Int) -> Text
    represent acc Nothing =
        T.concat
            [ acc
            , "    "
            ]
    represent acc (Just (score,maxval)) =
        T.concat
            [ acc
            , T.pack (show score)
            , "/"
            , T.pack (show maxval)
            , " "
            ]
