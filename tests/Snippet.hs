{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T

import Maturity.Scale

main :: IO ()
main = do
    T.putStrLn "Hello World"
