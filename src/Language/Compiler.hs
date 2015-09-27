{-# LANGUAGE OverloadedStrings #-}
module Language.Compiler where

import           Language.AST    (Entity (..))
import qualified Language.Parser as Parser
import           Text.Parsec     (ParseError)

-- Read a .prototype file and extract a list of entities
--
parseFile :: FilePath -> IO (Either ParseError [Entity])
parseFile f = do
   contents <- readFile f
   return $ Parser.parseExpr contents
