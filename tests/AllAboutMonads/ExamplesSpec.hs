module AllAboutMonads.ExamplesSpec(spec) where

import Test.Hspec
import qualified Data.Map as Map
import System.IO

type Dict = Map String String
type Entry = String

addEntry :: Dict -> Entry -> Dict
addEntry d e = insert e d

addDataFromFile :: Dict -> Handle -> IO Dict
addDataFromFile db handle =
  do content <- hGetContents handle
     let entries = map read $ lines content
     return $ foldl addEntry dict entries


openForReading fileName = openFile fileName ReadMode

files = ["one.txt", "two.txt", "three.txt"]
mapAllEntriesInFiles =
  do
     handle <- mapM openForReading files
     mapM $ addDataFromFile Map handlehandhand

spec :: Spec
spec = do
  it "example 4" $ do
    1 `shouldBe` 1

