module AllAboutMonads.ExamplesSpec(spec) where

import Test.Hspec
import Control.Monad
import System.IO
import Data.Tuple

splitAtFirstComma :: String -> (String, String)
splitAtFirstComma s = let (before, after) = break (== ',') s
                      in (before, drop 2 after)

swapNames :: String -> String
swapNames s = (\(a, b) -> b ++ " " ++ a) (splitAtFirstComma s)


fixName (a, b) = (a, swapNames b)

getName :: String -> Maybe String
getName name = do let db = [("John", "Smith, John"), ("Mike", "Caine, Michael")]
                  liftM swapNames (lookup name db)

spec :: Spec
spec = do
  it "swaps names" $ do
    swapNames "Rossi, Mario" `shouldBe` "Mario Rossi"

  it "fixes names" $ do
    fixName ("Mar", "Rossi, Mario") `shouldBe` ("Mar", "Mario Rossi")

  it "swap names" $ do
    liftM fixName [("John", "Smith, John"), ("Mike", "Caine, Michael")] `shouldBe` [("John", "John Smith"), ("Mike", "Michael Caine")]

