{-# LANGUAGE DeriveGeneric #-}
module Language.AST where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics

data PropertyKind = Optional
                  | Required
                  | Repeated
  deriving ( Eq, Ord, Show, Generic )

instance FromJSON PropertyKind
instance ToJSON PropertyKind

data PropertyType = Integer
                  | Float
                  | String
                  | Bool
                  | Ref String
  deriving ( Eq, Ord, Show, Generic )

instance FromJSON PropertyType
instance ToJSON PropertyType

data Property = Property {
    propertyKind :: PropertyKind
  , propertyType :: PropertyType
  , propertyName :: String
} deriving ( Eq, Ord, Show, Generic )

instance FromJSON Property
instance ToJSON Property

-- An entity representing some kind of domain object / model
--
data Entity = Entity {
    name       :: String
  , properties :: [Property]
} deriving ( Eq, Ord, Show, Generic )

instance FromJSON Entity
instance ToJSON Entity

