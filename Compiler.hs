module Compiler where

data Entity = Entity {
    name       :: String
  , properties :: [Property]
} deriving ( Show )

data Kind = Optional
          | Required
          | Repeated
  deriving ( Show )

data Type = Integer
          | Float
          | String
          | Boolean
          | Ref String
  deriving ( Show )

data Property = Property {
    propertyKind :: Kind
  , propertyType :: Type
  , propertyName :: String
} deriving ( Show )

card = Entity "Card" [ Property Required String "pan" ]

user = Entity "User" [ Property Required String "name"
                     , Property Repeated (Ref "Card") "cards"
                     ]

