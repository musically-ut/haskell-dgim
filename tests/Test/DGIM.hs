{-# LANGUAGE PackageImports, NamedFieldPuns #-}
module Test.DGIM ( tests ) where

import Data.Stream.Algorithms.DGIM.Internal as DGIM

import qualified Test.QuickCheck as Q
import Distribution.TestSuite as TS

toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {}         = TS.Pass
toTSResult Q.GaveUp {}          = TS.Fail "GaveUp"
toTSResult Q.Failure {Q.reason} = TS.Fail reason

runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
        qres <- Q.quickCheckWithResult Q.stdArgs {Q.maxSuccess = 30,
                                                  Q.maxSize = 20} prop
        return $ (Finished . toTSResult) qres

tests :: IO [Test]
tests = return [Test $ TestInstance (runQuickCheck checkAccuracy)
                                    "checkAccuracy" ["empty-tag"] [] undefined
                -- Test $ TestInstance (runQuickCheck propCheckP1)
                --                     "propCheckP1" [] [] undefined
                ]


data Elem = Zero | One deriving (Eq, Show)
instance Q.Arbitrary Elem where
  arbitrary = Q.elements [Zero, One]

type Stream = [Elem]

toNum :: Elem -> Integer
toNum Zero = 0
toNum One  = 1

countOnes :: Stream -> Integer
countOnes stream = sum $ map toNum stream

checkAccuracy :: Stream -> Bool
checkAccuracy stream =
  let accuracy = 0.99 in
  let streamLen = fromIntegral $ length stream in
  let truth = countOnes stream in
  let dgim = foldr DGIM.insert
                   (DGIM.mkDGIM accuracy streamLen (== One))
                   stream
             in
  let prediction = DGIM.query dgim streamLen in
  abs (fromIntegral $ truth - prediction) <= (1.0 - accuracy) * fromIntegral truth
