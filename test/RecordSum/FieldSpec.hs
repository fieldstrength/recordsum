module RecordSum.FieldSpec where

import           Prelude

import           Control.Lens.Combinators (view)
import           GHC.Generics             (Generic)
import           Hedgehog
import           RecordSum.Field          (recordSumField)

tests :: IO Bool
tests = checkParallel $$(discover)

data T1
  = MkT11 D1
  | MkT12 D2
  deriving stock (Generic)

data D1 = D1 {
  dField :: Int
} deriving stock (Generic)

data D2 = D2 {
  dField :: Int
} deriving stock (Generic)

prop_can_access_1 :: Property
prop_can_access_1 = property $ do
  let
    fVal = 1
    record = MkT11 $ D1 fVal
  view (recordSumField @"dField") record === fVal
