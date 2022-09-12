module Chapter3 where

data Gianca = Gianca Int

instance Eq Gianca where
    Gianca g1 == Gianca g2 = g1 == g2

g1 = Gianca 1
g2 = Gianca 2

main =
   print $ show( g1 == g2 )
