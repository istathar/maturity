{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


main :: IO ()
main = mainWith (bgFrame 0.1 white example)

example :: Diagram B

{-
example = circle 1 # fc blue
                   # lw veryThick
                   # lc purple
                   # dashingG [0.2,0.05] 0

example = dashingG [0.2,0.05] 0 (lc purple (lw veryThick (fc blue (circle 1))))

example = (dashingG [0.2,0.05] 0 . lc purple . lw veryThick . fc blue) circle 1

example = square 1 # fc aqua # showOrigin ||| circle 1

example = ell # snugR <> ell # snugL
  where
    ell = circle 1 # scaleX 0.5 # rotateBy (1/6)
    

example = hrule (2 * sum sizes) === circles # centerX
  where circles = hcat . map alignT . zipWith scale sizes
                $ repeat (circle 1)
        sizes   = [2,5,4,7,1,3]

-- A Worked Example

example = regPoly 6 1

-}

node :: Int -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white
    <> circle 0.2 # fc green # named n

arrowOpts = 
    with & gaps .~ small & headLength .~ local 0.15

tournament :: Int -> Diagram B
tournament n = atPoints (trailVertices (regPoly n 1)) (map node [1..n])
    # applyAll
        [connectOutside' arrowOpts j k | j <- [1..n-1], k <- [j+1..n]]
    # connectOutside' arrowOpts (1 :: Int) (2 :: Int)

example = tournament 6



