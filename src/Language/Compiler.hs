{-# LANGUAGE OverloadedStrings #-}
module Language.Compiler where

parseFile :: FilePath -> IO String
parseFile f = do
   contents <- readFile f
   return contents
