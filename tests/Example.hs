{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Example (example) where

import Data.Typeable
import Diagrams.Prelude
import Diagrams.TwoD.Text

example
  :: (Typeable n, RealFloat n,
      Renderable (Text n) b,
      Renderable (Path V2 n) b)
  => QDiagram b V2 n Any


--
-- Has to be in a `rect` (or a something, anything) because Text, by itself,
-- has no size, apparently
--
example = baselineText "Hello World" <> rect 8 1 # fc lightgreen # showOrigin

