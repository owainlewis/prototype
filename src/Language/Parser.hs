{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Parser
-- Copyright   :  2015 Owain Lewis
-- License     :  public domain

-- Maintainer  :  owain@owainlewis.com
-- Stability   :  experimental
-- Portability :  portable

-- Prototype language parser
-----------------------------------------------------------------------------
module Language.Parser
  ( parseExpr
  ) where

import           Control.Applicative ((*>), (<*))
import           Language.AST
import           Text.Parsec
import           Text.Parsec.String  (Parser)
import qualified Text.Parsec.Token   as T

word :: Parser String
word = many1 letter

lexme :: Parser a -> Parser a
lexme p = spaces *> p <* spaces

braces :: Parser a -> Parser a
braces p = do
    _ <- lexme $ string "{"
    r <- p
    _ <- lexme $ string "}"
    return r

parseRequired :: Parser PropertyKind
parseRequired = string "required" >> return Required

parseOptional :: Parser PropertyKind
parseOptional = string "optional" >> return Optional

parseRepeated :: Parser PropertyKind
parseRepeated = string "repeated" >> return Repeated

parsePropertyKind :: Parser PropertyKind
parsePropertyKind = try parseRequired <|> parseOptional <|> parseRepeated

parseIntType :: Parser PropertyType
parseIntType = parser >> return Integer
    where parser = string "integer" <|> string "int64" <|> string "int32"

parseFloatType :: Parser PropertyType
parseFloatType = string "float" >> return Float

parseStringType :: Parser PropertyType
parseStringType = string "string" >> return String

parseBooleanType :: Parser PropertyType
parseBooleanType = string "bool" >> return Bool

parseRef :: Parser PropertyType
parseRef = word >>= (\w -> return $ Ref w)

parsePropertyType :: Parser PropertyType
parsePropertyType =
    parseIntType     <|>
    parseFloatType   <|>
    parseStringType  <|>
    parseBooleanType <|>
    parseRef

parseProperty :: Parser Property
parseProperty = do
     propertyKind <- spaces *> parsePropertyKind
     propertyType <- lexme parsePropertyType
     propertyName <- word
     _ <-  lexme $ string ";"
     return $ Property propertyKind propertyType propertyName

parsePropertyLines :: Parser [Property]
parsePropertyLines = many parseProperty

parseEntity :: Parser Entity
parseEntity = do
    _          <- string "entity" <* spaces
    name       <- word <* spaces
    properties <- braces $ lexme parsePropertyLines
    return $ Entity name properties

parseEnum :: Parser Enumeration
parseEnum = do
    _ <- string "enum" <* spaces
    name       <- word <* spaces
    values     <- braces $ many (lexme word)
    return $ Enumeration name values

parseExpr :: String -> Either ParseError [Entity]
parseExpr s = parse (many parseEntity) "<stdin>" s
