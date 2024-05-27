module AllAboutMonads.SheepsSpec(spec) where

import Test.Hspec
import Control.Conditional(when, unless)
import Control.Monad ((>=>), MonadPlus, foldM)
import Control.Monad.State
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


sequence' :: Monad m => [m a] -> m [a]
sequence' []     = return []
sequence' (x:xs)=
  do v <- x
     rest <- sequence' xs
     return (v:rest)

sequence'' :: Monad m => [m a] -> m [a]
sequence'' =
  foldr mcons (return [])
   where mcons e acc = e >>= (\v -> acc >>= (\xsx -> return (v:xsx)))


-- foldM :: (Monad m) => (acc -> e -> m acc) -> acc -> [e] -> m acc
sequence''' :: Monad m => [m a] -> m [a]
sequence''' = foldM (\acc e ->
                       do ee <- e
                          return (ee : acc))
              []


twice :: Int -> State String Int
twice v = return (v * 2)


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

  it "sequences Maybes" $ do
    let maybes = [Just "Hey", Just "Joe", Just "!"]
    let prelude  = sequence  maybes
    let handmade = sequence' maybes
    prelude `shouldBe` handmade

  it "shows the usage of unless" $ do
    let result = do put "initial state"
                    when True (put "if-true")
                    twice 20
          in runState result "Hey!" `shouldBe` (40, "if-true")

