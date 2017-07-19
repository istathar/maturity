{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T

import Maturity.Scale
import Maturity.Rubric
import Text.Render (render)

model :: Model
model = Model
    (Technical CodeExists DesignChoices)
    (Operational Discussion Insecure Undefined)

main :: IO ()
main = do
    T.putStrLn (render model)
