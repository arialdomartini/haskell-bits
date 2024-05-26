module BookOfMonads.Chapter1Spec(spec) where

import Test.Hspec

eqList :: Eq a => [a] -> [a] -> Bool
eqList [] [] = True
eqList (a:as) (b:bs) = a == b && eqList as bs
eqList _ _ = False

spec :: Spec
spec = do
  it "equation for lists" $ do
    let l1 = [1,2,3,4] :: [Int]
    let l2 = [1,2,3,4] :: [Int]
    (l1 `eqList` l2) `shouldBe` True
