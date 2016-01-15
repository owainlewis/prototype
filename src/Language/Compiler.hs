module Language.Compiler where

class Renderable a where
    render :: a -> String
