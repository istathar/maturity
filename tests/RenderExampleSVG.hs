{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Example (example)

main :: IO ()
main = mainWith diagram

diagram :: Diagram B
diagram = (bgFrame 0.1 grey example)
