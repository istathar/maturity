{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Maturity.Types where

import Data.Text (Text)

{-
    A note about the language extenstions turned on above:

    We need MultiParamTypeClasses (well, ConstrainedClassMethods actually, but
    it's 2017) so that the Enum constraint can be located on `assign` rather
    than as a superclass. We need _that_ so that the technique of using orphan
    instances to hide the boilerplate machinery away from the "user" type
    declarations can be done.

    AllowAmbiguousTypes is what allows the `summary` function to be declared
    without reference to the typeclass constraint. It pairs with the
    TypeApplications extension used at the calling site.
-}

--
-- | A type which represents levels in a Rubric.
--
class Rubric a where
    --
    -- | A plain text title for the Rubric.
    --
    title :: Text

    --
    -- | An brief overview of the Rubric as a whole.
    --
    summary :: Text

    --
    -- | For each level in the Rubric, supply a long form textual description
    -- which will be used to fill in a box in a grid display.
    --
    description :: a -> Text

    --
    -- | Given a number, convert it to one of the levels in the underlying
    -- Enum. FIXME The input Double has to be in the range of 0.0 - maxBound.
    --
    assign :: Enum a => Double -> a
    assign = toEnum . truncate


