{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine

import Example (example)

main :: IO ()
main = mainWith diagram

diagram :: Diagram B
diagram = (bgFrame 0.1 grey example)
