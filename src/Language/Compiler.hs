module Language.Compiler where

import           Language.AST    (Entity (..))
import qualified Language.Parser as Parser

class Renderable a where
    render :: a -> String


