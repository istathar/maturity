{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Maturity.Component where

import Data.String.Here
import Text.Render (Render, render)

import Maturity.Types

--
-- | There are two axis, Technical Maturity and Operational Maturity. Both are
-- scored on a scale of 0 to 10 out of 10. Haskell Enums are zero origin so the
-- first entry in each Sum type is value 0.
--

data Model = Model TechnicalMaturity OperationalMaturity

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

instance Rubric ConceptualProgress where
    description Inception = [here|
There's an idea.
|]
    description WrittenDown = [here|
The idea has been written down; others can learn from it.
|]
    description CodeExists = [here|
The concept has progressed to actual code!
|]
    description OthersKnow = [here|
Others on the team know about the concept, and can see its code.
|]
    description TeamCanHack = [here|
The team as a whole can actually contribute to the code base.
|]

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

instance Rubric TechnicalProgress where
    description Beginning = [here|
Theoretical work that may develop into a Component has begun.
|]

    description DesignChoices = [here|
We've surveyed the available options,
a technology choice has been made, and
preliminary engineering design has been done.
|]

    description InstalledLocally = [here|
Someone has figured out how to install it locally on their workstation.
|]

    description ProvedConcept = [here|
A proof of concept has been done using the technology chosen, and
design of how we will use it has been validated.
|]

    description Deployable = [here|
We are now able to deploy this technology in our environment
using our automation, testing, and tooling.
|]

    description Modifiable = [here|
We have successfully adapted the technology to perform the
role we envisioned for it.
|]

    description Optimized = [here|
The service is in production. We are able to monitor its performance,
carry out tuning, manage its resource consumption, and
optomize its performance.
|]


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

instance Rubric CustomerViewpoint where
    description Discussion =
        "Preliminary discussion stage"
    description Premise =
        "Premise has been documented and the customer has accepted it"
    description Demo =
        "Customer has seen a demo"
    description Tried =
        "Customer has tried the component themselves"
    description Initial =
        "Sufficient functionality is present for initial use by customer"
    description Enough =
        "Customer has enough of what they want; they won't be interested further"

{-
    Raises an interesting question; what about _other_ customers?
-}


--
-- | What security and authentication controls are in place, 2 points
--
data SecurityLevel
    = Insecure
    | LocalFile
    | Enterprise


instance Rubric SecurityLevel where
    description Insecure = [here|
        No security measures implemented
    |]
    description LocalFile = [here|
        Access controlled by hard-coded local files
    |]
    description Enterprise = [here|
        Kerberos, Single Sign On, or other enterprise
        control for authentication in use
    |]


--
-- | Service Management constructs, 3 points
--
data ServiceManagement
    = Undefined
    | ServiceLevel
    | IncidentReponse
    | Budget

instance Rubric ServiceManagement where
    description Undefined =
        "No production service management implemented"
    description ServiceLevel =
        "Service Level Objectives and Contracts defined and agreed"
    description IncidentReponse =
        "Service Provider Group designated and active"
    description Budget =
        "Funding for ongoing sustainment of the Service in place"
