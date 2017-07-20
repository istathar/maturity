{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T

import Maturity.Scale

model :: Model
model = Model
    (Technical CodeExists DesignChoices)
    (Operational Discussion Insecure Undefined)

vault = Model
    (Technical OthersKnow Deployable)
    (Operational Demo Enterprise Undefined)

main :: IO ()
main = do
    putStrLn (show model)
    T.putStrLn (render model)
    putStrLn ""
    putStrLn (show vault)
    T.putStrLn (render vault)
