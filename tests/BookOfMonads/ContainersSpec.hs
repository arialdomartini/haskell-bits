module AllAboutMonads.ContainersSpec(spec) where

import Test.Hspec

newtype AppendLastList a = AppendLastList {getAppendLastList :: [a]} deriving (Eq, Show)

class Container c where
  empty :: c a
  insert :: c a -> a -> c a

instance Container [] where
  empty = []
  insert xs x = x:xs

instance Container AppendLastList where
  empty = AppendLastList []
  insert xs x =
    AppendLastList (getAppendLastList xs ++ [x])

spec :: Spec
spec = do
  it "defines an empty List" $ do
    let xs = empty  :: [Int]
    xs `shouldBe` []


  it "append to the head" $ do
    let xs = (empty `insert` "foo") `insert` "bar" in
      xs `shouldBe` ["bar", "foo"]


  it "append lasts" $ do
    let xs = (empty `insert` "foo") `insert` "bar" :: AppendLastList String in
      xs `shouldBe` AppendLastList ["foo", "bar"]
