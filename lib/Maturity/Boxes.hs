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
    vcat
        [ drawMaturityLabel "Technical Maturity"
        , drawRubricIntoBoxes conceptual
        , drawRubricIntoBoxes technical
        , drawMaturityLabel "Operational Maturity"
        , drawRubricIntoBoxes customer
        , drawRubricIntoBoxes security
        , drawRubricIntoBoxes service
        ]

drawMaturityLabel :: String -> Drawing b n
drawMaturityLabel headline =
    text headline
        # font "Liberation Sans"
        # fontSize (local 1)
        <> rect 24 1 # fc yellow

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
     => a -> Diagram b
drawRubricIntoBoxes (_ :: a) =
  let
    levels =
        enumFrom (minBound @a)
    boxes = 
        fmap (drawLevelIntoBox) levels
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

drawLevelIntoBox :: (Rubric a, Enum a, Bounded a) => a -> Drawing b n
drawLevelIntoBox (level :: a) =
  let
    paragraph = wrap 25 (description level)     -- WARNING MAGIC NUMBER
    maxval = fromEnum (maxBound @a)
    score = fromEnum level
    palette = fmap (tint 0.2) (brewerSet Greens (maxval + 1))
--  letters = reverse (brewerSet Greys (maxval + 1))
--  letters = [black, black, black, white, white, white]
    letters = repeat black
  in
    text (T.unpack paragraph)
        # fc (letters !! score)
        # font "Linux Libertine O"
        # fontSize (local 1)
        <> rect 18 14 # fc (palette !! score)



