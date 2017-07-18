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
    = Technical ConceptualProgress TechnicalProgress

data OperationalMaturity
    = Operational CustomerViewpoint SecurityLevel ServiceManagement

--
-- | How far has an idea progressed from inception to a team being able to work
-- on it and sustain it?
--
data ConceptualProgress
    = Inception
    | WrittenDown
    | CodeExists
    | OthersKnow
    | TeamCanHack
    deriving (Enum, Eq, Ord, Bounded, Show)

instance Render ConceptualProgress where
    render Inception =
        "There's an idea."
    render WrittenDown =
        "The idea has been written down; others can learn from it."
    render CodeExists =
        "The concept has progressed to actual code!"
    render OthersKnow =
        "Others on the team know about the concept, and can see its code."
    render TeamCanHack =
        "The team as a whole can actually contribute to the code base."

--
-- | How far has the project progressed from it's beginning through having been
-- adapted to our needs, in production, and being tuned & optimized.
--
data TechnicalProgress
    = Beginning
    | DesignChoices
    | InstalledLocally
    | ProvedConcept
    | Deployable
    | Modifiable
    | Optimized
    deriving (Enum, Eq, Ord, Bounded, Show)

instance Render TechnicalProgress where
    render Beginning =
        "Theoretical work that may develop into a Component has begun"
    render DesignChoices =
        "We've surveyed the available options. A technology choice has been made. Preliminary engineering design has been done."
    render InstalledLocally =
        "Someone has figured out how to install it locally on their workstation."
    render ProvedConcept =
        "A proof of concept has been done. The technology chosen and design of how we will use it has been validated."
    render Deployable =
        "We are now able to deploy this technology in our environment, using our automation, testing, and tooling."
    render Modifiable =
        "We have successfully adapted the technology to perform the role we envisioned for it."
    render Optimized =
        "The service is in production. We are able to monitor its performance, carry out tuning, manage its resource consumption, and optomize its performance."


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
