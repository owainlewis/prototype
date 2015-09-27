{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson           (encode)
import qualified Data.ByteString.Lazy as LBS
import           Language.AST
import qualified Language.Parser      as Parser
import           System.Environment   (getArgs)
import           Text.Parsec          (ParseError)

-- Read a .prototype file and extract a list of entities
--
parseFile :: FilePath -> IO (Either ParseError [Entity])
parseFile f = do
     contents <- readFile f
     return $ Parser.parseExpr contents

render :: Either ParseError [Entity] -> Maybe LBS.ByteString
render response =
  case response of
    Left  e -> Nothing
    Right r -> Just $ encode r

main :: IO ()
main = do
    args <- getArgs
    case args of
      [file] -> do
        parsed <- parseFile file
        case (render parsed) of
          Just r -> LBS.writeFile "data.json" r
          Nothing -> putStrLn "Failed to parse file"
      _ -> putStrLn "Use: ./run file.prototype"

