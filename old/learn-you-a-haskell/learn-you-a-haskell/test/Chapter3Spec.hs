module Chapter3Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

keepOnlyUpperCase :: String -> String
keepOnlyUpperCase s = s


spec::Spec
spec = do

    describe "Chapter 3 exercises" $ do
      it "removes non-uppercase" $ do
          property $ \s ->
            let
              originalLen = length s
              lenAfter = length $ keepOnlyUpperCase s
            in
              lenAfter <= originalLen
