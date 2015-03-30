{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Stream.Algorithms.DGIM.Internal (
  -- * Type (with constructors)
    DGIM(..)

  -- * Creation
  , mkDGIM

  -- * Insertion
  , insert
  , insert_

  -- * Querying
  , querySince
  , queryAll
  , queryLen

) where

import Control.Exception ( assert )

type Index        = Integer
type NumOnesLog2  = Integer
data Bucket       = B !Index !NumOnesLog2 deriving (Show)

getIndex :: Bucket -> Index
getIndex (B idx _) = idx

getCount :: Bucket -> Integer
getCount (B _ val) = 2 ^ val


-- TODO: The index can only increase by one at a time.
-- Should create a time-stamp dependent version and provide this as a simpler
-- interface.
data DGIM a = DGIM {
    dgimPredicate     :: !(a -> Bool)
  , dgimBuckets       :: ![Bucket]
  , dgimMaxSameBucket :: !Integer
  , dgimCliff         :: !Integer
  , dgimCurrentIdx    :: !Integer
  }

instance Show (DGIM a) where
    show DGIM{..} = "r = "       ++ (show dgimMaxSameBucket) ++ " " ++
                    "cliff = "   ++ (show dgimCliff        ) ++ " " ++
                    "idx = "     ++ (show dgimCurrentIdx   ) ++ " " ++
                    "buckets = " ++ (show dgimBuckets      )

mkDGIM :: (RealFrac a, Eq b) => a -> Integer -> (b -> Bool) -> DGIM b
mkDGIM accuracy k predicate =
    assert (accuracy >= 0.0 || accuracy < 1.0) $
      let numBuckets = ceiling (1 / (1 - accuracy)) in
      DGIM predicate [] numBuckets k 0

incrIndex :: DGIM a -> DGIM a
incrIndex dg = dg { dgimCurrentIdx = dgimCurrentIdx dg + 1 }

insert_ :: DGIM a -> DGIM a
insert_ = incrIndex

insert :: a -> DGIM a -> DGIM a
insert !v !dgim =
     let newElem = if dgimPredicate dgim v then [ B (dgimCurrentIdx dgim + 1) 0 ] else [] in
     incrIndex $ dgim { dgimBuckets = reverse $ (go newElem (dgimBuckets dgim) (fromIntegral $ length newElem) 0) }
   where
     go newBuckets oldBuckets similarBuckets lastBucketValue =
         case oldBuckets of
           -- Visited all the buckets
           [] -> newBuckets

           -- The rest of the buckets are past the time-zone cliff, drop them
           ((B bucketIdx _):_) | dgimCurrentIdx dgim - bucketIdx > dgimCliff dgim
             -> newBuckets

           -- Combine the rth and r+1th bucket into a larger bucket
           ((B idxR vR):(B _ vR1):rest) | similarBuckets  == dgimMaxSameBucket dgim - 1 &&
                                          lastBucketValue == vR &&
                                          vR              == vR1
             -> go newBuckets ((B idxR (lastBucketValue + 1)):rest) similarBuckets lastBucketValue

           -- See another bucket with the same value
           (x@(B _ vR):rest) | lastBucketValue == vR
             -> go (x:newBuckets) rest (similarBuckets + 1) lastBucketValue

           -- See a bucket with a different value
           (x@(B _ vR):rest) -- | lastBucketValue /= vR
             -> go (x:newBuckets) rest 1 vR


querySince :: DGIM a -> Integer -> Integer
querySince dgim since =
    sumAll $ filter ((>= since) . getIndex) $ dgimBuckets dgim
  where
    sumAll []         = 0
    sumAll [x]        = let c = getCount x  in 1 + ((c - 1) `div` 2)
    sumAll (x:y:rest) = getCount x + sumAll (y:rest)

queryAll :: DGIM a -> Integer
queryAll dgim = querySince dgim (dgimCurrentIdx dgim - dgimCliff dgim)

queryLen :: DGIM a -> Integer -> Integer
queryLen dgim numPastEntries = querySince dgim (dgimCurrentIdx dgim - numPastEntries + 1)
