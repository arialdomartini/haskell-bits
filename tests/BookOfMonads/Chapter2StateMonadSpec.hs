module BookOfMonads.Chapter2StateMonadSpec(spec) where

import Test.Hspec

data Tree = Leaf | Node (Tree, Tree)

countLeaves :: Tree -> Int
countLeaves tree = 1

spec :: Spec
spec = do
  it "counts the number of leaves" $ do
    let tree = Node (Leaf, Node (Leaf, Leaf))
        numberOfLeaves = countLeaves tree
    numberOfLeaves `shouldBe` 3
  
