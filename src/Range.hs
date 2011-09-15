{-# LANGUAGE DatatypeContexts #-}

module Range (
      Range(..),
      rangesOverlap,
      mergeRange,
      mergeRanges,
      fromRanges,
      fromMergedRanges
   ) where

import Data.Ord (comparing)
import Data.List (sortBy)

data Ord a => Range a
   = SingletonRange a
   | SpanRange a a
   deriving(Eq, Show)

rangesOverlap :: (Ord a) => Range a -> Range a -> Bool
rangesOverlap (SingletonRange a) (SingletonRange b) = a == b
rangesOverlap (SingletonRange a) (SpanRange x y) = isBetween a (x, y)
rangesOverlap (SpanRange x y) (SingletonRange a) = isBetween a (x, y)
rangesOverlap (SpanRange x y) (SpanRange a b) = isBetween x (a, b) || isBetween a (x, y)

isBetween :: (Ord a) => a -> (a, a) -> Bool
isBetween a (x, y) = x <= a && a <= y

mergeRange :: (Ord a) => Range a -> Range a -> Either (Range a, Range a) (Range a)
mergeRange r1 r2 = if rangesOverlap r1 r2
                     then Right $ assumeMerge r1 r2
                     else Left (r1, r2)
   where
      assumeMerge :: (Ord a) => Range a -> Range a -> Range a
      assumeMerge (SingletonRange _) x = x
      assumeMerge x (SingletonRange _) = x
      assumeMerge (SpanRange x y) (SpanRange a b) = SpanRange (min x a) (max y b)

mergeRanges :: (Ord a) => [Range a] -> [Range a]
mergeRanges ranges = mergeRangesHelper $ sortBy (comparing getLowest) ranges
   where
      mergeRangesHelper :: (Ord a) => [Range a] -> [Range a]
      mergeRangesHelper (x:y:xs) = case mergeRange x y of
                                     Left (a, b) -> a : mergeRangesHelper (b:xs)
                                     Right a -> mergeRangesHelper (a:xs)
      mergeRangesHelper xs = xs

      getLowest :: (Ord a) => Range a -> a
      getLowest (SingletonRange x) = x
      getLowest (SpanRange x _) = x

fromRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromRanges [] = []
fromRanges (SingletonRange x:xs) = x : fromRanges xs
fromRanges (SpanRange a b:xs) = [a..b] ++ fromRanges xs

fromMergedRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromMergedRanges = fromRanges . mergeRanges
