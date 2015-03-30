module Data.Stream.Algorithms.DGIM (
  -- * Type (no constructors)
    DGIM

  -- * External interface
  , mkDGIM

  , insert
  , insert_

  , querySince
  , queryAll
  , queryLen
) where

import Data.Stream.Algorithms.DGIM.Internal
