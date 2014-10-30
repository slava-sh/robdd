import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.OBDD.Reduced

main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.OBDD.Reduced"
  [ testCase "Dummy" $ True @?= True
  , testProperty "Dummy" $ \x -> let _ = x :: Bool in True
  ]
