{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T

import Maturity.Scale


vault = Model
    (Technical OthersKnow Deployable)
    (Operational Demo Enterprise Undefined)

myself = Model
    (Technical CodeExists DesignChoices)
    (Operational Discussion Insecure Undefined)


main :: IO ()
main = do
    T.putStrLn (describeModel vault)

    T.putStrLn "vault"
    T.putStrLn (outputScores vault)
    T.putStrLn ""
    T.putStrLn "myself"
    T.putStrLn (outputScores myself)

