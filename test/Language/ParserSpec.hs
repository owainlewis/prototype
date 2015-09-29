module Language.ParserSpec (main, spec) where

import           Test.Hspec

import           Language.AST
import qualified Language.Parser as P
import           Text.Parsec     (ParseError)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

card = "entity Card { }"

success :: Either ParseError [Entity] -> Bool
success (Right _) = True
success (Left  _) = False

spec :: Spec
spec = do
  describe "parsing" $ do
    it "should work" $ do
      let parsed = P.parseExpr card in
        (success parsed) `shouldBe` True
