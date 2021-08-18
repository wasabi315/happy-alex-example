{-# LANGUAGE ImportQualifiedPost #-}

module Errors
  ( Error (..),
    ErrorKind (..),
  )
where

import Codec.Binary.UTF8.String qualified as Utf8
import SrcLoc

data Error = Error
  { span :: Span,
    kind :: ErrorKind
  }
  deriving (Show)

data ErrorKind
  = ErrUnclosedComment
  | ErrUnknownToken String
  | ErrUnknownLexicalError
  | ErrUnknownParserError

instance Show ErrorKind where
  show ErrUnclosedComment = "Unclosed comment"
  show (ErrUnknownToken tok) = "Unknown token: \"" ++ tok ++ "\""
  show ErrUnknownLexicalError = "Unknown lexical error"
  show ErrUnknownParserError = "Unknown parser error"
