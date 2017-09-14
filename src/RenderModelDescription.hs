{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where


import Data.Text (Text)
import qualified Data.Text.IO as T
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Maturity.Scale


vault = Model
    (Technical OthersKnow Deployable)
    (Operational Demo Enterprise Undefined)

myself = Model
    (Technical CodeExists DesignChoices)
    (Operational Discussion Insecure Undefined)


main :: IO ()
main = mainWith diagram

{-
    It's necessary to force the type coming in from drawModelAsPage to
    Diagram B otherwise types don't infer. Argh.
-}

diagram :: Diagram B
diagram =
  let
    illustration :: Diagram B
    illustration = drawModelAsPage vault
--  illustration = circle 5 # lc green # fc lightblue
  in
    illustration
{-
    `atop`
    boundingRect illustration # lw (output 1)
-}
