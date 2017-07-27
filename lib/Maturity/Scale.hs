module Maturity.Scale 
(
    Model(..),
    TechnicalMaturity(..),
    OperationalMaturity(..),
    ConceptualProgress(..),
    TechnicalProgress(..),
    CustomerViewpoint(..),
    SecurityLevel(..),
    ServiceManagement(..),

    score,

    Rubric(..),
    display,

    Render(..)
)
where

import Maturity.Types
import Maturity.Component
import Maturity.Instances
import Maturity.Display
import Text.Render

score :: Model -> String
score = undefined

