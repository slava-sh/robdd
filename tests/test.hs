import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (not, and, or)

import Data.OBDD
import qualified Data.Map as Map

main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.OBDD.Reduced"
  [ testCase "1" $ (impl (var 1) (var 2))
      @?= Branch (Var 1) (Leaf True) (Branch (Var 2) (Leaf False) (Leaf True))
  -- Adapted from http://www.cs.otago.ac.nz/staffpriv/ok/COSC410/robdd.hs
  , testCase "O'Keefe 2" $ (or (var 1) (var 1)) @?= var 1
  , testCase "O'Keefe 3" $ (iff (impl (var 1) (var 2)) (or (not (var 1)) (var 2))) @?= true
  , testCase "O'Keefe 4" $ (iff (and (var 1) (var 1)) (var 1)) @?= true
  -- , testCase "O'Keefe 5" $ (and (and (and (iff (var 13) (or (and false (var 1)) (or (and false (var
  --           5)) (and (var 1) (var 5))))) (iff (var 9) (iff (iff false (not (var
  --           1))) (not (var 5))))) (and (and (iff (var 14) (or (and (var 13)
  --           (var 2)) (or (and (var 13) (var 6)) (and (var 2) (var 6)))))
  --           (iff (var 10) (iff (iff (var 13) (not (var 2))) (not (var 6)))))
  --           (and (and (iff (var 15) (or (and (var 14) (var 3)) (or (and (var
  --           14) (var 7)) (and (var 3) (var 7))))) (iff (var 11) (iff (iff
  --           (var 14) (not (var 3))) (not (var 7))))) (and (iff (var 16) (or
  --           (and (var 15) (var 4)) (or (and (var 15) (var 8)) (and (var 4)
  --           (var 8))))) (iff (var 12) (iff (iff (var 15) (not (var 4))) (not
  --           (var 8)))))))) (and (and (and (iff (var 17) (or (and (not false)
  --           (and (var 1) (var 5))) (and false (or (var 1) (var 5))))) (iff (var
  --           9) (or (and false (and (var 1) (var 5))) (or (and false (and (not (var
  --           1)) (not (var 5)))) (or (and (not false) (and (var 1) (not (var
  --           5)))) (and (not false) (and (not (var 1)) (var 5)))))))) (and (and
  --           (iff (var 18) (or (and (not (var 17)) (and (var 2) (var 6)))
  --           (and (var 17) (or (var 2) (var 6))))) (iff (var 10) (or (and
  --           (var 17) (and (var 2) (var 6))) (or (and (var 17) (and (not (var
  --           2)) (not (var 6)))) (or (and (not (var 17)) (and (var 2) (not
  --           (var 6)))) (and (not (var 17)) (and (not (var 2)) (var 6))))))))
  --           (and (and (iff (var 19) (or (and (not (var 18)) (and (var 3)
  --           (var 7))) (and (var 18) (or (var 3) (var 7))))) (iff (var 11)
  --           (or (and (var 18) (and (var 3) (var 7))) (or (and (var 18) (and
  --           (not (var 3)) (not (var 7)))) (or (and (not (var 18)) (and (var
  --           3) (not (var 7)))) (and (not (var 18)) (and (not (var 3)) (var
  --           7)))))))) (and (iff (var 20) (or (and (not (var 19)) (and (var
  --           4) (var 8))) (and (var 19) (or (var 4) (var 8))))) (iff (var 12)
  --           (or (and (var 19) (and (var 4) (var 8))) (or (and (var 19) (and
  --           (not (var 4)) (not (var 8)))) (or (and (not (var 19)) (and (var
  --           4) (not (var 8)))) (and (not (var 19)) (and (not (var 4)) (var
  --           8))))))))))) (iff (var 16) (var 20)))) @?= ??
  -- , testCase "O'Keefe 6" $ (and (and (and (iff (var 13) (or (and false true) (or (and false (var 5))
  --           (and true (var 5))))) (iff (var 9) (iff (iff false (not true)) (not (var
  --           5))))) (and (and (iff (var 14) (or (and (var 13) false) (or (and
  --           (var 13) (var 6)) (and false (var 6))))) (iff (var 10) (iff (iff
  --           (var 13) (not false)) (not (var 6))))) (and (and (iff (var 15) (or
  --           (and (var 14) false) (or (and (var 14) (var 7)) (and false (var 7)))))
  --           (iff (var 11) (iff (iff (var 14) (not false)) (not (var 7))))) (and
  --           (iff (var 16) (or (and (var 15) true) (or (and (var 15) (var 8))
  --           (and true (var 8))))) (iff (var 12) (iff (iff (var 15) (not true))
  --           (not (var 8)))))))) (and (and (and (iff (var 17) (or (and (not
  --           false) (and true (var 5))) (and false (or true (var 5))))) (iff (var 9) (or
  --           (and false (and true (var 5))) (or (and false (and (not true) (not (var 5))))
  --           (or (and (not false) (and true (not (var 5)))) (and (not false) (and (not
  --           true) (var 5)))))))) (and (and (iff (var 18) (or (and (not (var
  --           17)) (and false (var 6))) (and (var 17) (or false (var 6))))) (iff (var
  --           10) (or (and (var 17) (and false (var 6))) (or (and (var 17) (and
  --           (not false) (not (var 6)))) (or (and (not (var 17)) (and false (not (var
  --           6)))) (and (not (var 17)) (and (not false) (var 6)))))))) (and (and
  --           (iff (var 19) (or (and (not (var 18)) (and false (var 7))) (and (var
  --           18) (or false (var 7))))) (iff (var 11) (or (and (var 18) (and false
  --           (var 7))) (or (and (var 18) (and (not false) (not (var 7)))) (or
  --           (and (not (var 18)) (and false (not (var 7)))) (and (not (var 18))
  --           (and (not false) (var 7)))))))) (and (iff (var 20) (or (and (not
  --           (var 19)) (and true (var 8))) (and (var 19) (or true (var 8))))) (iff
  --           (var 12) (or (and (var 19) (and true (var 8))) (or (and (var 19)
  --           (and (not true) (not (var 8)))) (or (and (not (var 19)) (and true (not
  --           (var 8)))) (and (not (var 19)) (and (not true) (var 8)))))))))))
  --           (iff (var 16) (var 20)))) @?= ??
  -- , testCase "O'Keefe 7" $ iff (not (or (var 1) (or (var 2) (var 3))))
  --           (and (not (var 1)) (and (not (var 2)) (not (var 3)) )) @?= ??
  , testCase "O'Keefe 8" $ (iff (not (and (var 1) (and (var 2) (and (var 3) (and (var 4)
            (and (var 5) (and (var 6) (and (var 7) (and (var 8) (and (var 9)
            (and (var 10) (and (var 11) (and (var 12) (and (var 13) (and
            (var 14) (and (var 15) (var 16))))))))))))))))) (or (not (var
            1)) (or (not (var 2)) (or (not (var 3)) (or (not (var 4)) (or
            (not (var 5)) (or (not (var 6)) (or (not (var 7)) (or (not (var
            8)) (or (not (var 9)) (or (not (var 10)) (or (not (var 11)) (or
            (not (var 12)) (or (not (var 13)) (or (not (var 14)) (or (not
            (var 15)) (not (var 16)))))))))))))))))) @?= true
  , testCase "O'Keefe 9" $ (iff (not (or (var 1) (or (var 2) (or (var 3) (or (var 4) (or
            (var 5) (or (var 6) (or (var 7) (or (var 8) (or (var 9) (or (var
            10) (or (var 11) (or (var 12) (or (var 13) (or (var 14) (or (var
            15) (var 16))))))))))))))))) (and (not (var 1)) (and (not (var
            2)) (and (not (var 3)) (and (not (var 4)) (and (not (var 5))
            (and (not (var 6)) (and (not (var 7)) (and (not (var 8)) (and
            (not (var 9)) (and (not (var 10)) (and (not (var 11)) (and (not
            (var 12)) (and (not (var 13)) (and (not (var 14)) (and (not (var
            15)) (not (var 16)))))))))))))))))) @?= true
  , testCase "Pengelly 10" $ (and (var 1) (not (var 1))) @?= false
  , testCase "Pengelly 11" $
      (or (and (var 1) (not (var 2))) (and (not (var 1)) (var 2)))
      @?= (xor (var 1) (var 2))
  , testCase "Pengelly 12" $ (and (and (var 1)
            (impl (var 1) (var 2))) (not (var 2))) @?= false
  , testCase "Pengelly 13" $ (and (and (not (var 2)) (impl (var 1) (var 2))) (var 1)) @?= false
  , testCase "Pengelly 14" $ (iff (var 1) (not (not (var 1)))) @?= true
  , testCase "Pengelly 15" $ (iff (and (and (var 1) (var 2)) (var 3))
            (and (var 1) (and (var 2) (var 3)))) @?= true
  , testCase "Pengelly 16" $ (iff (not (or (var 1) (var 2))) (and (not (var 1)) (not (var 2)))) @?= true
  , testCase "Pengelly 17" $
      (impl (impl (var 1) (var 3))
            (impl (impl (var 2) (var 4)) (impl (or (var 1) (var 2)) (var 3))))
      @?= (or (or (or (var 1) (not (var 2))) (var 3)) (not (var 4)))
  , testCase "Pengelly 18" $ (and (and (and (or (or (var 1) (var 2)) (not (var 3)))
            (or (or (var 1) (var 2)) (var 3))) (or (var 1) (not (var 2))))
            (not (var 1))) @?= false
  , testCase "Pengelly 19" $ (impl (var 1) (impl (var 2) (var 1))) @?= true
  , testCase "anySat of a tautology"    $ anySat true  @?= Just Map.empty
  , testCase "allSat of a tautology"    $ allSat true  @?= [Map.empty]
  , testCase "anySat of a contradicion" $ anySat false @?= Nothing
  , testCase "allSat of a contradicion" $ allSat false @?= []
  ]

true :: OBDD
true = constant True

false :: OBDD
false = constant False
