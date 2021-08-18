module AST where

import SrcLoc

type Expr = Located Expr_

data Expr_
  = Let String Expr Expr
  | Op Op Expr Expr
  | Int Int
  | Var String
  deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)
