import TestGeom
import TestIntegrate
import TestHashTable
import TestFS

import Test.Tasty

main :: IO ()
main = defaultMain $
         testGroup
           "Tests"
           [ testGeom
           , testIntegrate
           , testHashTable
           , testFS
            ]
