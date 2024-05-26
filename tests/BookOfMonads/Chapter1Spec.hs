module BookOfMonads.Chapter1Spec(spec) where

import Test.Hspec

class Equatable a where
  eq :: a -> a -> Bool

eqList :: Eq a => [a] -> [a] -> Bool
eqList [] [] = True
eqList (a:as) (b:bs) = a == b && eqList as bs
eqList _ _ = False

instance Eq a => Equatable [a] where
  eq = eqList

spec :: Spec
spec = do
  it "equality for lists" $ do
    let l1 = [1,2,3,4] :: [Int]
    let l2 = [1,2,3,4] :: [Int]
    (l1 `eqList` l2) `shouldBe` True

  it "equality for lists using a type class" $ do
    let l1 = [1,2,3,4] :: [Int]
    let l2 = [1,2,3,4] :: [Int]
    (l1 `eq` l2) `shouldBe` True

