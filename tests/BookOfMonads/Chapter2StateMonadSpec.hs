module BookOfMonads.Chapter2StateMonadSpec(spec) where

import Test.Hspec

data Tree v = Leaf v | Node (Tree v)  (Tree v)
  deriving (Show, Eq)

countLeaves :: Tree v -> Int
countLeaves (Leaf _)     = 1
countLeaves (Node l r)   = countLeaves l + countLeaves r

convert :: (String->Int) ->Tree String -> Tree Int
convert f (Leaf v)     = Leaf (f v)
convert f (Node l r) = Node (convert f l) (convert f r)

spec :: Spec
spec = do
  it "counts the number of leaves" $ do
    let tree = Node (Leaf ()) (Node (Leaf ()) (Leaf ()) )
        numberOfLeaves = countLeaves tree
    numberOfLeaves `shouldBe` 3

  it "maps leaves" $ do
    let treeOfWords = Node (Leaf "one") (Node (Leaf "two") (Leaf "three"))
        treeOfNumbs = Node (Leaf 3) (Node (Leaf 3) (Leaf 5 ))
        treeOfLengths = convert length treeOfWords
    treeOfLengths `shouldBe` treeOfNumbs

