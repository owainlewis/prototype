{-# LANGUAGE DeriveDataTypeable #-}
module Language.AST where

import           Data.Data
import           Data.Generics

data PropertyKind = Optional
                  | Required
                  | Repeated
  deriving ( Eq, Ord, Show, Data, Typeable )

data PropertyType = Integer
                  | Float
                  | String
                  | Boolean
                  | Ref String
  deriving ( Eq, Ord, Show, Data, Typeable )

data Property = Property {
    propertyKind :: PropertyKind
  , propertyType :: PropertyType
  , propertyName :: String
} deriving ( Eq, Ord, Show, Data, Typeable )

-- An entity representing some kind of domain object / model
--
data Entity = Entity {
    entityName       :: String
  , entityProperties :: [Property]
} deriving ( Eq, Ord, Show, Data, Typeable )
