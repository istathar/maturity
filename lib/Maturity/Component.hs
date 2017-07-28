{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Maturity.Component where

import Data.String.Here
import Maturity.Types

--
-- | There are two axis, Technical Maturity and Operational Maturity. Both are
-- scored on a scale of 0 to 10 out of 10. Haskell Enums are zero origin so the
-- first entry in each sum type is value 0.
--

data Model = Model TechnicalMaturity OperationalMaturity

data TechnicalMaturity
    = Technical ConceptualProgress TechnicalProgress

data OperationalMaturity
    = Operational CustomerViewpoint SecurityLevel ServiceManagement


data ConceptualProgress
    = Inception
    | WrittenDown
    | CodeExists
    | OthersKnow
    | TeamCanHack

instance Rubric ConceptualProgress where
    title = "Conceptual Progress"

    summary = [here|
        How far has an idea progressed from inception
        to a team being able to work on it and sustain
        it?
    |]

    description Inception = [here|
        There exists an idea.
    |]
    description WrittenDown = [here|
        The idea has been written down; others can
        learn from it.
    |]
    description CodeExists = [here|
        The concept has progressed to actual code!
    |]
    description OthersKnow = [here|
        Others on the team know about the concept,
        can see its code, and can read documentation.
    |]
    description TeamCanHack = [here|
        The team as a whole can actually contribute
        to the code base.
    |]


data TechnicalProgress
    = Beginning
    | DesignChoices
    | InstalledLocally
    | ProvedConcept
    | Deployable
    | Modifiable
    | Optimized

instance Rubric TechnicalProgress where
    title = "Technical Progress"

    summary = [here|
        How far has the project progressed from it's
        beginning through having been adapted to our
        needs, in production, and being tuned &
        optimized.
    |]

    description Beginning = [here|
        Theoretical work that may develop into a
        Component has begun.
    |]

    description DesignChoices = [here|
        We've surveyed the available options,
        a technology choice has been made, and
        preliminary engineering design has been
        completed.
    |]

    description InstalledLocally = [here|
        Someone has figured out how to install it
        locally on their workstation.
    |]

    description ProvedConcept = [here|
        A proof of concept has been done using the
        technology chosen, and design of how we will
        use it has been validated.
    |]

    description Deployable = [here|
        We are now able to deploy this technology in
        our environment using our automation, testing,
        and tooling.
    |]

    description Modifiable = [here|
        We have successfully adapted the technology to
        perform the role we envisioned for it.
    |]

    description Optimized = [here|
        The service is in production. We are able to
        monitor its performance, carry out tuning,
        manage its resource consumption, and optimize
        its performance.
    |]


data CustomerViewpoint
    = Discussion
    | Premise
    | Demo
    | Tried
    | Initial
    | Enough

instance Rubric CustomerViewpoint where
    title = "Customer's Viewpoint"

    summary = [here|
        The degree of completeness of a Component, from
        a customer's point of view
    |]

    description Discussion = [here|
        Preliminary discussion stage.
    |]
    description Premise = [here|
        Premise has been documented and the customer
        has accepted it.
    |]
    description Demo = [here|
        Customer has seen a demo!
    |]

    description Tried = [here|
        Customer has tried the component themselves.
    |]
    description Initial = [here|
        Sufficient functionality is present for
        initial use by customer.
    |]
    description Enough = [here|
        Customer has enough of what they want; they
        won't be interested further.
    |]

{-
    Raises an interesting question; what about _other_ customers?
-}


data SecurityLevel
    = Insecure
    | LocalFile
    | Enterprise

instance Rubric SecurityLevel where
    title = "Security"

    summary = [here|
        What security and authentication controls are
        in place?
    |]

    description Insecure = [here|
        No security measures are implemented.
    |]
    description LocalFile = [here|
        Access is controlled, but only by hard-coded
        local files controlled by the team that wrote
        the component.
    |]
    description Enterprise = [here|
        Kerberos, single sign on, or some other
        enterprise-grade mechanism for authentication
        in use.
    |]


--
--
data ServiceManagement
    = Undefined
    | ServiceLevel
    | IncidentReponse
    | Budget

instance Rubric ServiceManagement where
    title = "Service Management"

    summary = [here|
        Degree to which the initiative behind this
        component has evolved into a mature service
        that can be run in production.
    |]

    description Undefined = [here|
        No production service management implemented.
    |]
    description ServiceLevel = [here|
        Service Level Objectives and boundary contracts
        are defined and agreed.
    |]
    description IncidentReponse = [here|
        Service Provider Group designated and active.
    |]
    description Budget = [here|
        Funding for this component to be run as an
        ongoing service is in place.
    |]

