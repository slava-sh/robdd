import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (not, and, or, const)

import Data.OBDD.Reduced
import qualified Data.OBDD.Reduced.Internal as I
import qualified Data.Map as Map

main = defaultMain tests

tests :: TestTree
tests = testGroup "Data.OBDD.Reduced"
  --[ testCase "1" $ (impl (var 1) (var 2))
  --    @?= I.Branch (I.Var 1) (I.Leaf True)
  --          (I.Branch (I.Var 2) (I.Leaf False) (I.Leaf True))
  -- Adapted from http://www.cs.otago.ac.nz/staffpriv/ok/COSC410/robdd.hs
  [ testROBDD "O'Keefe 2" (or (var 1) (var 1)) (var 1)
  , testROBDD "O'Keefe 3" (iff (impl (var 1) (var 2)) (or (not (var 1)) (var 2))) true
  , testROBDD "O'Keefe 4" (iff (and (var 1) (var 1)) (var 1)) true
  , testROBDD "O'Keefe 7"
      (iff (not (or (var 1) (or (var 2) (var 3))))
           (and (not (var 1)) (and (not (var 2)) (not (var 3))))) true
  , testROBDD "O'Keefe 8"
      (iff (not (and (var 1) (and (var 2) (and (var 3) (and (var 4)
            (and (var 5) (and (var 6) (and (var 7) (and (var 8) (and (var 9)
            (and (var 10) (and (var 11) (and (var 12) (and (var 13) (and
            (var 14) (and (var 15) (var 16))))))))))))))))) (or (not (var
            1)) (or (not (var 2)) (or (not (var 3)) (or (not (var 4)) (or
            (not (var 5)) (or (not (var 6)) (or (not (var 7)) (or (not (var
            8)) (or (not (var 9)) (or (not (var 10)) (or (not (var 11)) (or
            (not (var 12)) (or (not (var 13)) (or (not (var 14)) (or (not
            (var 15)) (not (var 16)))))))))))))))))) true
  , testROBDD "O'Keefe 9" (iff (not (or (var 1) (or (var 2) (or (var 3) (or (var 4) (or
            (var 5) (or (var 6) (or (var 7) (or (var 8) (or (var 9) (or (var
            10) (or (var 11) (or (var 12) (or (var 13) (or (var 14) (or (var
            15) (var 16))))))))))))))))) (and (not (var 1)) (and (not (var
            2)) (and (not (var 3)) (and (not (var 4)) (and (not (var 5))
            (and (not (var 6)) (and (not (var 7)) (and (not (var 8)) (and
            (not (var 9)) (and (not (var 10)) (and (not (var 11)) (and (not
            (var 12)) (and (not (var 13)) (and (not (var 14)) (and (not (var
            15)) (not (var 16)))))))))))))))))) true
  , testROBDD "Pengelly 10" (and (var 1) (not (var 1))) false
  , testROBDD "Pengelly 11"
      (or (and (var 1) (not (var 2))) (and (not (var 1)) (var 2)))
      (xor (var 1) (var 2))
  , testROBDD "Pengelly 12" (and (and (var 1) (impl (var 1) (var 2))) (not (var 2))) false
  , testROBDD "Pengelly 13" (and (and (not (var 2)) (impl (var 1) (var 2))) (var 1)) false
  , testROBDD "Pengelly 14" (iff (var 1) (not (not (var 1)))) true
  , testROBDD "Pengelly 15"
      (iff (and (and (var 1) (var 2)) (var 3))
           (and (var 1) (and (var 2) (var 3)))) true
  , testROBDD "Pengelly 16"
      (iff (not (or (var 1) (var 2))) (and (not (var 1)) (not (var 2)))) true
  , testROBDD "Pengelly 17"
      (impl (impl (var 1) (var 3))
            (impl (impl (var 2) (var 4)) (impl (or (var 1) (var 2)) (var 3))))
      (or (or (or (var 1) (not (var 2))) (var 3)) (not (var 4)))
  , testROBDD "Pengelly 18"
      (and (and (and (or (or (var 1) (var 2)) (not (var 3)))
                     (or (or (var 1) (var 2)) (var 3)))
                (or (var 1) (not (var 2))))
           (not (var 1)))
      false
  , testROBDD "Pengelly 19" (impl (var 1) (impl (var 2) (var 1))) true
  , testCase "anySat of a tautology"    $ anySat (const True)  @?= Just Map.empty
  , testCase "allSat of a tautology"    $ allSat (const True)  @?= [Map.empty]
  , testCase "anySat of a contradicion" $ anySat (const False) @?= Nothing
  , testCase "allSat of a contradicion" $ allSat (const False) @?= []
  ]

testROBDD s x y = testCase s $ runIdPoolM $ do
  rx <- reduce x
  ry <- reduce y
  return $ rx @?= ry
