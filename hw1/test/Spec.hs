import TestDays
import TestNat
import TestPlants
import TestFoldable
import TestSplitting
import TestConcats
import TestAlgclasses

import Test.Tasty

main :: IO ()
main =
    defaultMain $
      testGroup
        "Tests"
        [ testDays
        , testNat
        , testPlants
        , testFoldable
        , testSplitting
        , testConcats
        , testAlgclasses
        ]

