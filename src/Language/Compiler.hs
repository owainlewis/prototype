module Language.Compiler where

import           Text.Parsec
import           Text.Parsec.String (Parser)

data Entity = Entity {
    entityName       :: String
  , entityProperties :: [Property]
} deriving ( Show )

data PropertyKind = Optional
                  | Required
                  | Repeated
  deriving ( Eq, Ord, Show )

data PropertyType = Integer
                  | Float
                  | String
                  | Boolean
                  | Ref String
  deriving ( Eq, Ord, Show )

data Property = Property {
    propertyName :: String
  , propertyKind :: PropertyKind
  , propertyType :: PropertyType
} deriving ( Eq, Ord, Show )

-- Examples

card = Entity "Card" [ Property "pan" Required String ]

user = Entity "User" [ Property "name" Required String
                     , Property "cards" Repeated (Ref "Card")
                     ]

parseRequired :: Parser PropertyKind
parseRequired = string "required" >> return Required

parseOptional :: Parser PropertyKind
parseOptional = string "optional" >> return Optional

parseRepeated :: Parser PropertyKind
parseRepeated = string "repeated" >> return Repeated

parsePropertyKind :: Parser PropertyKind
parsePropertyKind = try parseRequired <|> parseOptional <|> parseRepeated

parseExpr :: String -> Either ParseError PropertyKind
parseExpr s = parse parsePropertyKind "<stdin>" s

parseFile :: FilePath -> IO String
parseFile f = do
   contents <- readFile f
   return contents

go = parseFile "user.prototype"


