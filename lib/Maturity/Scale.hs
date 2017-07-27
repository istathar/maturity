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

    Render(..)
)
where

import Maturity.Types
import Maturity.Component
import Maturity.Instances
import Text.Render

score :: Model -> String
score = undefined

