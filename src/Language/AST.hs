{-# LANGUAGE DeriveGeneric #-}
module Language.AST where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics

data PropertyKind = Optional
                  | Required
                  | Repeated
  deriving ( Eq, Show, Generic )

instance FromJSON PropertyKind
instance ToJSON PropertyKind

data PropertyType = Integer
                  | Float
                  | String
                  | Bool
                  | Ref String
  deriving ( Eq, Show, Generic )

instance FromJSON PropertyType
instance ToJSON PropertyType

data Property = Property {
    propertyKind :: PropertyKind
  , propertyType :: PropertyType
  , propertyName :: String
} deriving ( Eq, Show, Generic )

instance FromJSON Property
instance ToJSON Property

data Entity = Entity {
    entityName       :: String
  , entityProperties :: [Property]
} deriving ( Eq, Show, Generic )

instance FromJSON Entity
instance ToJSON Entity

data Enumeration = Enumeration {
    enumName   :: String
  , enumValues :: [String]
} deriving ( Eq, Show, Generic)

instance FromJSON Enumeration
instance ToJSON Enumeration
