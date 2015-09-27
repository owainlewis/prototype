module Language.AST where

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
    propertyKind :: PropertyKind
  , propertyType :: PropertyType
  , propertyName :: String
} deriving ( Eq, Ord, Show )

-- Examples

card = Entity "Card" [ Property Required String "pan" ]

user = Entity "User" [ Property Required String "name"
                     , Property Repeated (Ref "Card") "cards"
                     ]

