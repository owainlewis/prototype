{-# LANGUAGE OverloadedStrings #-}
module Language.Compiler where

import           Control.Applicative ((*>), (<*))
import           Language.AST
import           Text.Parsec
import           Text.Parsec.String  (Parser)
import qualified Text.Parsec.Token   as T

parseRequired :: Parser PropertyKind
parseRequired = string "required" >> return Required

parseOptional :: Parser PropertyKind
parseOptional = string "optional" >> return Optional

parseRepeated :: Parser PropertyKind
parseRepeated = string "repeated" >> return Repeated

parsePropertyKind :: Parser PropertyKind
parsePropertyKind = try parseRequired <|> parseOptional <|> parseRepeated

-- Property Types

parseIntType :: Parser PropertyType
parseIntType = string "integer" >> return Integer

parseFloatType :: Parser PropertyType
parseFloatType = string "float" >> return Float

parseStringType :: Parser PropertyType
parseStringType = string "string" >> return String

parseBooleanType :: Parser PropertyType
parseBooleanType = string "boolean" >> return Boolean

parseRef :: Parser PropertyType
parseRef = do
    ref <- word
    return $ Ref ref

parsePropertyType :: Parser PropertyType
parsePropertyType =
  parseIntType     <|>
  parseFloatType   <|>
  parseStringType  <|>
  parseBooleanType <|>
  parseRef

-- Words
word :: Parser String
word = many1 letter

-- Properties
parseProperty :: Parser Property
parseProperty = do
   propertyKind <- parsePropertyKind
   _ <- spaces
   propertyType <- parsePropertyType
   _ <- spaces
   propertyName <- word
   _ <- string ";"
   return $ Property propertyKind propertyType propertyName

go :: Parser Property -> String -> Maybe Property
go p s = case parse p "<stdin>" s of
    Left e  -> Nothing
    Right v -> Just v

parseExpr :: String -> Either ParseError PropertyKind
parseExpr s = parse parsePropertyKind "<stdin>" s

parseFile :: FilePath -> IO String
parseFile f = do
   contents <- readFile f
   return contents

-- go = parseFile "user.prototype"


