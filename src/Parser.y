{

{-# LANGUAGE ViewPatterns #-}

module Parser where

import AST
import Errors
import Lexer
import Monad
import SrcLoc

}

%name exprParser
%tokentype { Located Token }

%monad { Parser } { (>>=) } { pure }
%lexer { lexer } { At _ TEOF }

%token
  let    { At _ TLet }
  in     { At _ TIn }
  int    { (atTInt -> Just $$) }
  var    { (atTVar -> Just $$) }
  '+'    { At _ TAdd }
  '-'    { At _ TSub }
  '*'    { At _ TMul }
  '/'    { At _ TDiv }
  '='    { At _ TEq }
  '('    { At _ TLParen }
  ')'    { At _ TRParen }

%%

Expr :: { Expr }
  : let var '=' Expr in Expr    { At (mergeSpan (loc $1) (loc $6)) (Let (val $2) $4 $6) }
  | Expr1                       { $1 }

Expr1 :: { Expr }
  : Expr1 '+' Term              { At (mergeSpan (loc $1) (loc $3)) (Op Add $1 $3) }
  | Expr1 '-' Term              { At (mergeSpan (loc $1) (loc $3)) (Op Sub $1 $3) }
  | Term                        { $1 }

Term :: { Expr }
  : Term '*' Factor             { At (mergeSpan (loc $1) (loc $3)) (Op Mul $1 $3) }
  | Term '/' Factor             { At (mergeSpan (loc $1) (loc $3)) (Op Div $1 $3) }
  | Factor                      { $1 }

Factor :: { Expr }
  : int                         { fmap Int $1 }
  | var                         { fmap Var $1 }
  | '(' Expr ')'                { $2 }

{

atTInt (At sp (TInt n)) = Just (At sp n)
atTInt _ = Nothing

atTVar (At sp (TVar v)) = Just (At sp v)
atTVar _ = Nothing

happyError = do
  input <- getInput
  let sp = Span (offset input) (offset input)
  throw $ Error sp ErrUnknownParserError

}
