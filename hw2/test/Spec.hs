import TestStringSum
import TestTreeInst
import TestNonEmpty
import TestArithmetic
import TestSMA
import TestParsers

import Test.Tasty

main :: IO ()
main =
    defaultMain $
      testGroup
        "Tests"
        [ testStringSum
        , testTreeInst
        , testNonEmpty
        , testArithmetic
        , testSMA
        , testParsers
        ]