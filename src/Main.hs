{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import           System.Environment

import           Language.AST       (Entity (..))
import qualified Language.Parser    as Parser
import           Text.Parsec        (ParseError)

-- Read a .prototype file and extract a list of entities
--
parseFile :: FilePath -> IO (Either ParseError [Entity])
parseFile f = do
     contents <- readFile f
     return $ Parser.parseExpr contents

main = do
    args <- getArgs
    case args of
      [file] -> do
        x <- parseFile file
        case x of
          Left  e -> putStrLn $ "Failed to parse " ++ (show e)
          Right r -> putStrLn $ show r
      _ -> putStrLn "Use: ./run file.prototype"

