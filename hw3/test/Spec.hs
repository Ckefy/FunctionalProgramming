import Test.Tasty
import TestsFile

main :: IO ()
main = 
   defaultMain $
         testGroup
           "Tests"
           [ myTests ]
