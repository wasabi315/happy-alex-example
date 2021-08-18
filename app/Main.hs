{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as T
import Monad
import Parser

main :: IO ()
main = do
  txt <- T.getContents
  print $ parse exprParser txt
