module RecordSum.FieldSpec where

import           Hedgehog

tests :: IO Bool
tests = checkParallel $$(discover)

prop_1 :: Property
prop_1 = property $ True === True
