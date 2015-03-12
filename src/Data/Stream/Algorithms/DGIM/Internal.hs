module Data.Stream.Algorithms.DGIM.Internal (
  -- * Type (with constructors)
    DGIM(..)

  -- * External interface
  , mkDGIM
  , insert
  , query
) where

import qualified Data.Dequeue  as DQ
import qualified Data.Foldable as F

import Control.Exception ( assert )

type Index = Integer
type NumOnesLog10 = Integer
data Bucket = B !Index !NumOnesLog10 deriving (Show)

getIndex :: Bucket -> Index
getIndex (B idx _) = idx

getCount :: Bucket -> Integer
getCount (B _ val) = 2 ^ val

data DGIM a = DGIM {
    -- TODO: Generalize to a predicate function
    dgimPredicate     :: !(a -> Bool)
  , dgimBuckets       :: !(DQ.BankersDequeue Bucket)
  , dgimMaxSameBucket :: !Integer
  , dgimCliff         :: !Integer
  , dgimCurrentIdx    :: !Integer
  }

mkDGIM :: (RealFrac a, Eq b) => a -> Integer -> (b -> Bool) -> DGIM b
mkDGIM accuracy k predicate =
    assert (accuracy >= 0.0 || accuracy < 1.0) $
      let numBuckets = ceiling (1 / (1 - accuracy)) in
      DGIM predicate DQ.empty numBuckets k 0


partitionEq :: DQ.BankersDequeue Bucket
            -> Integer
            -> Maybe (Bucket, DQ.BankersDequeue Bucket)
partitionEq bucketsQ k =
    case DQ.popFront bucketsQ of
      (Nothing, _) -> Nothing
      (Just b, remainingBuckets) ->
          case go b remainingBuckets 0 of
            (l, lastBucket, restQ) | l == k - 1 -> Just (lastBucket, restQ)
            _                                   -> Nothing
  where
    go lastBucket@(B _ val) q l =
      case DQ.popFront q of
        _ | l == (k - 1)  -> (l, lastBucket, q)
        (Just newBucket@(B _ newVal), restQ) | val == newVal
                          -> go newBucket restQ (l + 1)
        _                 -> (l, lastBucket, q)


insert :: a -> DGIM a -> DGIM a
insert v dgim =
    let dgimNew = dropLastIfNeeded $ incrIndex dgim in
    if dgimPredicate dgim v
      then addOne dgimNew
      else dgimNew
  where
    incrIndex dg = dg { dgimCurrentIdx = dgimCurrentIdx dg + 1 }
    melt r bs = case partitionEq bs r of
      Nothing -> bs
      Just (B lastIdx lastCountLog2, rest) ->
        melt r $ DQ.pushFront rest (B lastIdx (lastCountLog2 + 1))
    addOne dg =
        let (buckets, idx, r) = (dgimBuckets dg, dgimCurrentIdx dg, dgimMaxSameBucket dg) in
        dg { dgimBuckets = melt r $ DQ.pushFront buckets (B idx 1) }
    dropLastIfNeeded dg =
        case DQ.last $ dgimBuckets dg of
            Nothing           -> dg
            Just (B endIdx _) ->
                if (dgimCurrentIdx dg - endIdx) > dgimCliff dg then
                    dg { dgimBuckets = snd $ DQ.popBack $ dgimBuckets dg }
                else dg

query :: DGIM a -> Integer -> Integer
query dgim till =
    sumAll $ filter ((<= till) . getIndex) $ F.toList $ dgimBuckets dgim
  where
    sumAll []         = 0
    sumAll [x]        = getCount x `div` 2
    sumAll (x:y:rest) = getCount x + sumAll (y:rest)

