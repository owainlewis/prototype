module Language.ParserSpec (main, spec) where

import           Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing" $ do
    it "work" $ do
      1 `shouldBe` (1 :: Int)
