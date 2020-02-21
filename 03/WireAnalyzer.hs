module WireAnalyzer where

import Data.Complex
import Data.Ix
import qualified Data.List as DL

-- | A wire segment is represented ad a pair of ranges, one referring to the horizontal space occupied, the other to the vertical one
data Segment r = Segment {hRange :: r, vRange :: r} deriving Show

instance Functor Segment where
    fmap f (Segment x y) = Segment (f x) (f y)

-- | Motions are represented by subsequent sums of complex numbers
type PathElem n = Complex n

type Point c = (c, c)

loadPathElems :: (RealFloat a, Read a) => String -> [PathElem a]
loadPathElems s = map readElem $ fragment s
    where fragment s = let replComma ',' = ' '
                           replComma c = c
                        in words . map replComma $ s -- From CSV to space-separated values
          readElem (s:ss) = (case s of
                               'U' -> 0 :+ 1
                               'D' -> 0 :+ (-1)
                               'L' -> (-1) :+ 0
                               'R' -> 1 :+ 0) * ((read ss) :+ 0) -- Single motion is calculated as direction (phase) times stride (modulo)

loadCable :: (RealFloat a, Integral i) => [PathElem a] -> [Segment (i, i)]
loadCable pes = map (fmap normRange) $ layTrack (0 :+ 0) pes -- Starting point: 0 + 0i
    where normRange r@(a, b) = if a > b then (b, a) else r -- Reformat ranges as (lower, upper)
          layTrack _ [] = []
          layTrack ip (el:els) = let fp = ip + el -- Final point = starting point + motion
                                     truncateReal = truncate . realPart
                                     truncateImag = truncate . imagPart
                                  in Segment (truncateReal ip, truncateReal fp) (truncateImag ip, truncateImag fp) : layTrack fp els

-- | Find out if two segments intersect
intersectsWith :: (Ix i) => Segment (i, i) -> Segment (i, i) -> Bool
intersectsWith s1 s2 = rangeOverlaps (hRange s1) (hRange s2) && rangeOverlaps (vRange s1) (vRange s2)
    -- Two segments intersect when both of their space ranges overlap
        where rangeOverlaps (i1, f1) (i2, f2) = (i1 <= f2) && (f1 >= i2)

-- | Find the intersection point between two segments
intersectionPoints :: (Ix p) => Segment (p, p) -> Segment (p, p) -> [Point p]
intersectionPoints s1 s2 = zipLongest interH interV
    where interH = DL.intersect (range $ hRange s1) (range $ hRange s2)
          interV = DL.intersect (range $ vRange s1) (range $ vRange s2)
          zipLongest l1 l2 = if length l1 > length l2 then
                                                      zip l1 (cycle l2)
                                                      else
                                                      zip (cycle l1) l2

-- | Calculate the Manhattan distance between two points
mHDistance :: (Integral c) => Point c -> Point c -> c
mHDistance (x1, y1) (x2, y2) = (abs $ x2 - x1) + (abs $ y2 - y1)
