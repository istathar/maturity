{-# OPTIONS_HADDOCK hide, not-home #-}

module Maturity.Rubric where

import Data.Text (Text)
import Text.Render (Render)

--
-- | There are two axis, Technical Maturity and Operational Maturity.
--

model :: (Technical, Operational)
model = undefined

data Technical = Technical 

data Operational = Operational Customer Security Management

data Customer = Discussion | Premise | Demo | Tried | Initial | Enough 
    deriving (Enum, Show)

instance Render Customer where
    render Discussion =
        "Preliminary discussion stage"
    render Premise =
        "Premise has been documented and the customer has accepted it"
    render Demo =
        "Customer has seen a demo"
    render Tried =
        "Customer has tried the component themselves"
    render Initial =
        "Sufficient functionality is present for initial use by customer"
    render Enough =
        "Customer has enough of what they want; they won't be interested further"


data Security = None | LocalFile | Enterprise



data Management = Undefined | ServiceLevelAgreement | ServiceProviderGroup | Budget
