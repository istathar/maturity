{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Maturity.Boxes
(
    drawModelAsPage,
    drawRubricIntoBoxes,
    drawLevelIntoBox
) where

import Data.Colour.Palette.BrewerSet
import Data.Colour.Palette.Harmony
import qualified Data.Text as T
import Data.Typeable
import Diagrams.Prelude
import Diagrams.TwoD.Text
import Text.Render (wrap)

import Maturity.Types
import Maturity.Component
import Maturity.Instances ()


drawModelAsPage :: Model -> Drawing b n
drawModelAsPage
    (Model
        (Technical conceptual technical)
        (Operational customer security service)
    ) =
    frame 2 $ vcat
        [ drawMaturityLabel "Technical Maturity"
        , hcat' (with & sep .~ 5) [
          drawRubricIntoBoxes conceptual Greens
        , drawRubricIntoBoxes technical Blues]
        , drawMaturityLabel "Operational Maturity"
        , hcat' (with & sep .~ 5) [
          drawRubricIntoBoxes customer Purples
        , drawRubricIntoBoxes security Reds
        , drawRubricIntoBoxes service YlOrBr]
        ]

drawMaturityLabel :: String -> Drawing b n
drawMaturityLabel headline =
    baselineText headline
        # font "Linux Libertine O"
        # fontSize (local 5) # translate (r2 (-9,-2))
        <> rect 0 12 # lineWidth 0

--
-- | Put the descriptive meta information about Rubric type into 
-- horizontally joined boxes.
--
{-
    The scoped type annotation on `rubric` is necessary to bring type a into
    scope at the term level (so we can then use TypeApplications). Go figure.
-}
drawRubricIntoBoxes
    :: (Rubric a,
        Enum a,
        Bounded a,
        V b ~ V2,
        N b ~ n,
        Typeable n,
        RealFloat n,
        Renderable (Path V2 n) b,
        Renderable (Text n) b)
     => a -> ColorCat -> Diagram b
drawRubricIntoBoxes (_ :: a) palette =
  let
    levels =
        enumFrom (minBound @a)
    boxes = 
        fmap (drawLevelIntoBox palette) levels
  in
    hcat boxes

--
-- | Given a Rubric value, output a wrapped version of its description in a Box.
-- The ordinal number of the box is an offset into the colour set being used
-- for that Rubric.
--

type Drawing b n = 
    ( V b ~ V2
    , N b ~ n
    , Typeable n
    , RealFloat n
    , Renderable (Path V2 n) b
    , Renderable (Text n) b
    ) => QDiagram b (V b) (N b) Any

drawLevelIntoBox :: (Rubric a, Enum a, Bounded a) => ColorCat -> a -> Drawing b n
drawLevelIntoBox palette (level :: a) =
  let
    paragraph = wrap 25 (description level)     -- WARNING MAGIC NUMBER
    maxval = fromEnum (maxBound @a)
    score = fromEnum level
    colours = fmap (tint 0.05) (brewerSet palette (maxval + 1))
    letters = repeat black

    box = text (T.unpack paragraph)
            # fc (letters !! score)
            # font "Nimbus Sans L"
            # fontSize (local 1)
            <>
            rect 19 15 # fc (colours !! score)

    ord = topLeftText (show score)
            # font "Nimbus Sans L"
            # fontSize (local 2)
            # translate (r2 (-9,2))
            <>
            rect 1 6 # lineWidth 0
  in
    box
    ===
    ord



