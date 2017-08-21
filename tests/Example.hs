{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Example (example) where

import Data.String.Here
import qualified Data.Text as T
import Data.Typeable
import Diagrams.Prelude
import Diagrams.TwoD.Text

import Text.Render (wrap)

example
  :: (Typeable n, RealFloat n,
      Renderable (Text n) b,
      Renderable (Path V2 n) b)
  => QDiagram b V2 n Any


{-
str = "Hello for Apéx GO01"
This is a test of the Emergency Broadcast System.
MMMM MM M MMMM MM MMM MMMMMMM MMMMMMMMM 123456789 MMMMMMM
-}

str = [here|
We've surveyed the available options, a technology
choice has been made, and preliminary engineering
design has been completed.
|]

enbox :: String -> String
enbox = T.unpack . wrap 30 . T.pack


--
-- Has to be in a `rect` (or a something, anything) because Text, by itself,
-- has no size, apparently
--
example = text (enbox str) # fc blue 
    # font "Linux Libertine O"
    # fontSize (local 1)
    <> rect 20 15 # fc lightgreen # showOrigin

{-
    These font names worked. Tempted to say these are
    PangoFontDescription font family names, but I'm not
    certain.


    # font "Linux Libertine O"
    # font "DejaVu Serif"
    # font "DejaVu Sans"
    # font "Liberation Serif"
    # font "Linux Biolinum O"
    # font "Linux Libertine O"
    # font "Linux Libertine Initials O"
    # font "Linux Libertine Mono O"
    # font "Inconsolata"
    # font "Nimbus Sans L"
    # font "URW Gothic L"

    -- really?!?

    # font "TeX Gyre Heros"
-}
