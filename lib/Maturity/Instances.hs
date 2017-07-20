{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

{- 
    Stash the various instances for the model types as orphans here so we
    can attempt to keep the declarative file cleaner.
-}

module Maturity.Instances where

import qualified Data.Text as T
import Text.Render (Render, render)

import Maturity.Rubric

deriving instance Show      Model 
deriving instance Eq        Model 
deriving instance Ord       Model

instance Render Model where
    render (Model technical operational) =
        T.intercalate "\n" [ render technical, render operational]

deriving instance Show      TechnicalMaturity 
deriving instance Eq        TechnicalMaturity 
deriving instance Ord       TechnicalMaturity

instance Render TechnicalMaturity where
    render (Technical conceptual progress) =
        T.intercalate "\n" [render conceptual, render progress]

deriving instance Show      OperationalMaturity 
deriving instance Eq        OperationalMaturity 
deriving instance Ord       OperationalMaturity

instance Render OperationalMaturity where
    render (Operational customer security service) =
        T.intercalate "\n" [ render customer, render security, render service]

deriving instance Show      ConceptualProgress
deriving instance Enum      ConceptualProgress
deriving instance Eq        ConceptualProgress
deriving instance Ord       ConceptualProgress
deriving instance Bounded   ConceptualProgress

deriving instance Show      TechnicalProgress
deriving instance Enum      TechnicalProgress
deriving instance Eq        TechnicalProgress
deriving instance Ord       TechnicalProgress
deriving instance Bounded   TechnicalProgress

deriving instance Show      CustomerViewpoint
deriving instance Enum      CustomerViewpoint
deriving instance Eq        CustomerViewpoint
deriving instance Ord       CustomerViewpoint
deriving instance Bounded   CustomerViewpoint

deriving instance Show      SecurityLevel
deriving instance Enum      SecurityLevel
deriving instance Eq        SecurityLevel
deriving instance Ord       SecurityLevel
deriving instance Bounded   SecurityLevel

deriving instance Show      ServiceManagement
deriving instance Enum      ServiceManagement
deriving instance Eq        ServiceManagement
deriving instance Ord       ServiceManagement
deriving instance Bounded   ServiceManagement


