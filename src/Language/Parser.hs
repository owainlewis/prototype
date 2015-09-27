{-# LANGUAGE OverloadedStrings #-}
module Language.Parser where

import           Control.Applicative ((*>), (<*))
import           Language.AST
import           Text.Parsec
import           Text.Parsec.String  (Parser)
import qualified Text.Parsec.Token   as T

word :: Parser String
word = many1 letter

-- Properties

parseRequired :: Parser PropertyKind
parseRequired = string "required" >> return Required

parseOptional :: Parser PropertyKind
parseOptional = string "optional" >> return Optional

parseRepeated :: Parser PropertyKind
parseRepeated = string "repeated" >> return Repeated

parsePropertyKind :: Parser PropertyKind
parsePropertyKind = try parseRequired <|> parseOptional <|> parseRepeated

parseIntType :: Parser PropertyType
parseIntType = string "integer" >> return Integer

parseFloatType :: Parser PropertyType
parseFloatType = string "float" >> return Float

parseStringType :: Parser PropertyType
parseStringType = string "string" >> return String

parseBooleanType :: Parser PropertyType
parseBooleanType = string "boolean" >> return Boolean

parseRef :: Parser PropertyType
parseRef = word >>= (\w -> return $ Ref w)

parsePropertyType :: Parser PropertyType
parsePropertyType =
  parseIntType     <|>
  parseFloatType   <|>
  parseStringType  <|>
  parseBooleanType <|>
  parseRef

-- Properties
parseProperty :: Parser Property
parseProperty = do
   propertyKind <- spaces *> parsePropertyKind
   _ <- spaces
   propertyType <- parsePropertyType
   _ <- spaces
   propertyName <- word
   _ <- spaces *> string ";"
   return $ Property propertyKind propertyType propertyName

parsePropertyLines :: Parser [Property]
parsePropertyLines = parseProperty `sepBy` newline

go :: Parser [Property] -> String -> Maybe [Property]
go p s = case parse p "<stdin>" s of
    Left e  -> Nothing
    Right v -> Just v

parseExpr :: String -> Either ParseError PropertyKind
parseExpr s = parse parsePropertyKind "<stdin>" s
