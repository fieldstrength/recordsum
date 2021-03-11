module Main where

import           RecordSum.FieldSpec as FieldSpec

import           Hedgehog.Main       (defaultMain)

main :: IO ()
main = defaultMain [FieldSpec.tests]
