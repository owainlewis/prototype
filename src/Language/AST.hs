module Language.AST where

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

-- An entity representing some kind of domain object / model
--
data Entity = Entity {
    entityName       :: String
  , entityProperties :: [Property]
} deriving ( Eq, Ord, Show )
