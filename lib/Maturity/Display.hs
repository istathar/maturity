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
    headline =
        underline '-' (title @a)
    maxScore =
        T.pack (show (fromEnum (maxBound @a)))
    value =
        T.concat ["(", maxScore, " points)"]
    levels =
        enumFrom (minBound @a)
    descriptions = 
        fmap (wrap 55 . description) levels
  in
    T.intercalate "\n\n"
        (T.concat [headline,"\n",value] : descriptions)

