module AllAboutMonads.SheepsSpec(spec) where

import Test.Hspec
import Control.Monad ((>=>), MonadPlus)
import GHC.Base (mplus, mzero)

newtype Sheep = Sheep String deriving (Show, Eq)

father :: MonadPlus m => Sheep -> m Sheep
father (Sheep "mother") = return (Sheep "granfather")
father _                = mzero

mother :: MonadPlus m => Sheep -> m Sheep
mother (Sheep "sheep") = return (Sheep "mother")
mother _               = mzero

maternalGranfather :: Sheep -> Maybe Sheep
maternalGranfather sheep =
  do m <- mother sheep
     father m


maternalGranfatherWithoutDo :: Sheep -> Maybe Sheep
maternalGranfatherWithoutDo =
--  return sheep >>= mother >>= father
--    join . (fmap father) . mother
    mother Control.Monad.>=> father


parent :: MonadPlus m => Sheep -> m Sheep
parent sheep = father sheep `mplus` mother sheep


parents :: MonadPlus m => Sheep -> m Sheep
parents sheep =
  father sheep `mplus` mother sheep

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


  it "mplus" $ do
    parent (Sheep "sheep") `shouldBe` Just (Sheep "mother")
