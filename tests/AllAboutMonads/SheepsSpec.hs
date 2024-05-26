module AllAboutMonads.SheepsSpec(spec) where

import Test.Hspec
import Control.Monad ((<=<))

newtype Sheep = Sheep String deriving (Show, Eq)

father :: Sheep -> Maybe Sheep
father (Sheep "mother") = Just (Sheep "granfather")
father _        = Nothing

mother :: Sheep -> Maybe Sheep
mother (Sheep "sheep") = Just (Sheep "mother")
mother _       = Nothing

maternalGranfather :: Sheep -> Maybe Sheep
maternalGranfather sheep =
  do m <- mother sheep
     father m


maternalGranfatherWithoutDo :: Sheep -> Maybe Sheep
maternalGranfatherWithoutDo =
--  return sheep >>= mother >>= father
--    join . (fmap father) . mother
    father <=< mother

spec :: Spec
spec = do
  it "sheep with a granfather" $ do
    maternalGranfather (Sheep "sheep") `shouldBe` Just (Sheep "granfather")

  it "sheep without a granfather" $ do
    maternalGranfather (Sheep "other") `shouldBe` Nothing

  it "sheep with a granfather without do notation" $ do
    maternalGranfatherWithoutDo (Sheep "sheep") `shouldBe` Just (Sheep "granfather")

  it "sheep without a granfather without do notation" $ do
    maternalGranfatherWithoutDo (Sheep "other") `shouldBe` Nothing
