{

{-# LANGUAGE ImportQualifiedPost #-}

module Lexer where

import Control.Monad
import Data.Text.Lazy qualified as T
import Monad
import Errors
import SrcLoc

}

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

<0>                  $white+                           ;

<0>                  "--".*                            ;
<0, stateComment>    "{-"                              { enterComment }
<stateComment>       "-}"                              { leaveComment }
<stateComment>       .                                 ;
<stateComment>       \n                                ;

<0>                  "let"                             { special TLet }
<0>                  "in"                              { special TIn }
<0>                  $digit+                           { token (TInt . read) }
<0>                  "+"                               { special TAdd }
<0>                  "-"                               { special TSub }
<0>                  "*"                               { special TMul }
<0>                  "/"                               { special TDiv }
<0>                  "="                               { special TEq }
<0>                  "("                               { special TLParen }
<0>                  ")"                               { special TRParen }
<0>                  $alpha [$alpha $digit \_ \']*     { token TVar }

<0>                  .                                 { lexError ErrUnknownToken }
<0>                  \n                                ;

{

data Token
  = TLet
  | TIn
  | TInt Int
  | TVar String
  | TAdd
  | TSub
  | TMul
  | TDiv
  | TEq
  | TLParen
  | TRParen
  | TEOF
  deriving (Eq, Show)

type Action = AlexInput -> Int -> Parser (Located Token)

token :: (String -> Token) -> Action
token f input len = do
  let sp = Span (offset input) (offset input + len)
      str = T.unpack . T.take (fromIntegral len) . rest $ input
  pure $ At sp (f str)

special :: Token -> Action
special = token . const

lexError :: (String -> ErrorKind) -> Action
lexError f input len = do
  let sp = Span (offset input) (offset input + len)
      str = T.unpack . T.take (fromIntegral len) . rest $ input
  throw $ Error sp (f str)

enterComment :: Action
enterComment _ _ = do
  cd <- getCommentDepth
  setCommentDepth (cd + 1)
  setCode stateComment
  munch

leaveComment :: Action
leaveComment _ _ = do
  cd <- getCommentDepth
  setCommentDepth (cd - 1)
  when (cd == 1) $ do
    setCode 0
  munch

munch :: Parser (Located Token)
munch = do
  input <- getInput
  code <- getCode

  case alexScan input code of
    AlexEOF -> do
      let sp = Span (offset input) (offset input)

      commentDepth <- getCommentDepth
      when (commentDepth > 0) $ do
        throw $ Error sp ErrUnclosedComment

      pure $ At sp TEOF

    AlexError _ -> do
      let sp = Span (offset input) (offset input)
      throw $ Error sp ErrUnknownLexicalError

    AlexSkip input' _ -> do
      setInput input'
      munch

    AlexToken input' len action -> do
      setInput input'
      action input len

munchAll :: Parser [Located Token]
munchAll = loop []
  where
    loop toks = do
      tok@(At _ kind) <- munch
      let toks' = tok : toks
      if kind == TEOF
        then pure $ reverse toks'
        else loop toks'

lexer :: (Located Token -> Parser a) -> Parser a
lexer k = munch >>= k

}
