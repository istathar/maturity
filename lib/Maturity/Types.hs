{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Maturity.Types where

import Data.Text (Text)

--
-- | A type which represents levels in a Rubric.
--
class Rubric a where

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

