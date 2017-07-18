{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Maturity.Rubric where

import Data.Text (Text)
import Text.Render (Render, render)

--
-- | There are two axis, Technical Maturity and Operational Maturity.
--

model :: (TechnicalMaturity, OperationalMaturity)
model = undefined

data TechnicalMaturity
    = Technical

data OperationalMaturity
    = Operational CustomerViewpoint SecurityLevel ServiceManagement

--
-- | The degree of completeness of a Component, from a customer's point of view
--
data CustomerViewpoint
    = Discussion
    | Premise
    | Demo
    | Tried
    | Initial
    | Enough
    deriving (Enum, Eq, Ord, Bounded, Show)

instance Render CustomerViewpoint where
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


--
-- | What security and authentication controls are in place, 2 points
--
data SecurityLevel
    = Insecure
    | LocalFile
    | Enterprise
    deriving (Enum, Eq, Ord, Bounded, Show)


instance Render SecurityLevel where
    render Insecure =
        "No security measures implemented"
    render LocalFile =
        "Access controlled by hard-coded local files"
    render Enterprise =
        "Kerberos, Single Sign On, or other enterprise control for authentication in use"


--
-- | Service Management constructs, 3 points
--
data ServiceManagement
    = Undefined
    | ServiceLevel
    | IncidentReponse
    | Budget
    deriving (Enum, Eq, Ord, Bounded, Show)

instance Render ServiceManagement where
    render Undefined =
        "No production service management implemented"
    render ServiceLevel =
        "Service Level Objectives and Contracts defined and agreed"
    render IncidentReponse =
        "Service Provider Group designated and active"
    render Budget =
        "Funding for ongoing sustainment of the Service in place"
