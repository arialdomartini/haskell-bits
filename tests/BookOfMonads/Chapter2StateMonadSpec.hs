module BookOfMonads.Chapter2StateMonadSpec(spec) where

import Test.Hspec

data Tree = Leaf | Node Tree Tree
  deriving (Show, Eq)

countLeaves :: Tree -> Int
countLeaves Leaf        = 1
countLeaves (Node l r) = countLeaves l + countLeaves r

spec :: Spec
spec = do
  it "counts the number of leaves" $ do
    let tree = Node Leaf (Node Leaf Leaf)
        numberOfLeaves = countLeaves tree
    numberOfLeaves `shouldBe` 3

