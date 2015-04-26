{-# LANGUAGE PackageImports, NamedFieldPuns #-}
module Test.DGIM ( tests ) where

import qualified Data.Stream.Algorithms.DGIM.Internal as DGIM

import qualified Test.QuickCheck as Q
import qualified Distribution.TestSuite as TS

toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {}         = TS.Pass
toTSResult Q.GaveUp {}          = TS.Fail "GaveUp"
toTSResult Q.Failure {Q.reason} = TS.Fail reason

runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
        qres <- Q.quickCheckWithResult Q.stdArgs {Q.maxSuccess = 30,
                                                  Q.maxSize = 20} prop
        return $ (TS.Finished . toTSResult) qres

tests :: IO [TS.Test]
tests = return [TS.Test $ TS.TestInstance (runQuickCheck checkAccuracy)
                                    "checkAccuracy" ["empty-tag"] [] undefined
                -- TS.Test $ TS.TestInstance (runQuickCheck propCheckP1)
                --                     "propCheckP1" [] [] undefined
                ]

-------------------
-- Set up the tests
-------------------

data Elem = Zero | One deriving (Eq, Show)

instance Q.Arbitrary Elem where
  arbitrary = Q.elements [Zero, One]

type Stream = [Elem]

toNum :: Elem -> Integer
toNum Zero = 0
toNum One  = 1

countOnes :: Stream -> Int -> Integer
countOnes stream queryLen = sum $ map toNum $ take queryLen stream

checkAccuracy :: Stream -> Bool
checkAccuracy stream =
  let accuracy = 0.99 in
  let streamLen = fromIntegral $ length stream in
  let queryLen = streamLen `div` 2 in
  let truth    = countOnes stream queryLen in
  let dgim = foldr DGIM.insert
                   (DGIM.mkDGIM accuracy (fromIntegral queryLen) (== One))
                   stream in
  let prediction = DGIM.queryLen dgim (fromIntegral queryLen) in
  abs (fromIntegral $ truth - prediction) <= (1.0 - accuracy) * fromIntegral truth
