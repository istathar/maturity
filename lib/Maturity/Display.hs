{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Maturity.Display where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Render (Render, render, wrap, underline)

import Maturity.Types
import Maturity.Component
import Maturity.Instances

--
-- | Display descriptive meta information about a Rubric type.
--
{-
    The scoped type annotation on `rubric` is necessary to bring type a into
    scope at the term level (so we can then use TypeApplications). Go figure.
-}
display :: (Rubric a, Enum a, Bounded a) => a -> Text
display (rubric :: a) =
  let
    maxScore =
        T.pack (show (fromEnum (maxBound @a)))
    value =
        T.concat ["(", maxScore, " points)"]
    levels =
        enumFrom (minBound @a)
    descriptions = 
        fmap (wrap 55 . description) levels
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
describeMaturity title =
    T.concat
        [ title
        , " (10 points total)"
        , "\n"
        , underline '=' title
        , "\n"
        ]

