import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (not, and, or, const)

import Data.ROBDD
import Data.ROBDD.Expr
import qualified Data.ROBDD.Internal as I
import qualified Data.Map as Map

main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.ROBDD"
  -- Adapted from http://www.cs.otago.ac.nz/staffpriv/ok/COSC410/Robdd.hs
  [ testRobdd "O'Keefe 2" (or (var 1) (var 1)) (var 1)
  , testRobdd "O'Keefe 3" (iff (impl (var 1) (var 2)) (or (not (var 1)) (var 2))) true
  , testRobdd "O'Keefe 4" (iff (and (var 1) (var 1)) (var 1)) true
  , testRobdd "O'Keefe 7"
      (iff (not (or (var 1) (or (var 2) (var 3))))
           (and (not (var 1)) (and (not (var 2)) (not (var 3))))) true
  , testRobdd "O'Keefe 8"
      (iff (not (and (var 1) (and (var 2) (and (var 3) (and (var 4)
            (and (var 5) (and (var 6) (and (var 7) (and (var 8) (and (var 9)
            (and (var 10) (and (var 11) (and (var 12) (and (var 13) (and
            (var 14) (and (var 15) (var 16))))))))))))))))) (or (not (var
            1)) (or (not (var 2)) (or (not (var 3)) (or (not (var 4)) (or
            (not (var 5)) (or (not (var 6)) (or (not (var 7)) (or (not (var
            8)) (or (not (var 9)) (or (not (var 10)) (or (not (var 11)) (or
            (not (var 12)) (or (not (var 13)) (or (not (var 14)) (or (not
            (var 15)) (not (var 16)))))))))))))))))) true
  , testRobdd "O'Keefe 9" (iff (not (or (var 1) (or (var 2) (or (var 3) (or (var 4) (or
            (var 5) (or (var 6) (or (var 7) (or (var 8) (or (var 9) (or (var
            10) (or (var 11) (or (var 12) (or (var 13) (or (var 14) (or (var
            15) (var 16))))))))))))))))) (and (not (var 1)) (and (not (var
            2)) (and (not (var 3)) (and (not (var 4)) (and (not (var 5))
            (and (not (var 6)) (and (not (var 7)) (and (not (var 8)) (and
            (not (var 9)) (and (not (var 10)) (and (not (var 11)) (and (not
            (var 12)) (and (not (var 13)) (and (not (var 14)) (and (not (var
            15)) (not (var 16)))))))))))))))))) true
  , testRobdd "Pengelly 10" (and (var 1) (not (var 1))) false
  , testRobdd "Pengelly 11"
      (or (and (var 1) (not (var 2))) (and (not (var 1)) (var 2)))
      (xor (var 1) (var 2))
  , testRobdd "Pengelly 12" (and (and (var 1) (impl (var 1) (var 2))) (not (var 2))) false
  , testRobdd "Pengelly 13" (and (and (not (var 2)) (impl (var 1) (var 2))) (var 1)) false
  , testRobdd "Pengelly 14" (iff (var 1) (not (not (var 1)))) true
  , testRobdd "Pengelly 15"
      (iff (and (and (var 1) (var 2)) (var 3))
           (and (var 1) (and (var 2) (var 3)))) true
  , testRobdd "Pengelly 16"
      (iff (not (or (var 1) (var 2))) (and (not (var 1)) (not (var 2)))) true
  , testRobdd "Pengelly 17"
      (impl (impl (var 1) (var 3))
            (impl (impl (var 2) (var 4)) (impl (or (var 1) (var 2)) (var 3))))
      (or (or (or (var 1) (not (var 2))) (var 3)) (not (var 4)))
  , testRobdd "Pengelly 18"
      (and (and (and (or (or (var 1) (var 2)) (not (var 3)))
                     (or (or (var 1) (var 2)) (var 3)))
                (or (var 1) (not (var 2))))
           (not (var 1)))
      false
  , testRobdd "Pengelly 19" (impl (var 1) (impl (var 2) (var 1))) true
  , testCase "anySat of a tautology"    $ anySat (I.Leaf True)  @?= Just Map.empty
  , testCase "allSat of a tautology"    $ allSat (I.Leaf True)  @?= [Map.empty]
  , testCase "anySat of a contradicion" $ anySat (I.Leaf False) @?= Nothing
  , testCase "allSat of a contradicion" $ allSat (I.Leaf False) @?= []
  ]

testRobdd s x y = testCase s $ runRobddM $ do
  rx <- reduce x
  ry <- reduce y
  return $ rx @?= ry
