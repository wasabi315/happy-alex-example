{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Monad where

import Codec.Binary.UTF8.String qualified as Utf8
import Data.Text.Lazy qualified as T
import Data.Word
import Errors

data AlexInput = AlexInput
  { offset :: Int,
    prev :: Char,
    bytes :: [Word8],
    rest :: T.Text
  }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input =
  case (bytes input, T.uncons $ rest input) of
    (b : bs, _) -> Just (b, input {bytes = bs})
    ([], Nothing) -> Nothing
    ([], Just (c, cs)) ->
      let b : bs = Utf8.encode [c]
          offset' = offset input + 1
          input' = AlexInput offset' c bs cs
       in Just (b, input')

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar AlexInput {prev} = prev

data ParserState = ParserState
  { input :: AlexInput,
    code :: Int,
    commentDepth :: Int
  }

initialState :: T.Text -> ParserState
initialState t =
  ParserState
    { input =
        AlexInput
          { prev = '\n',
            bytes = [],
            rest = t,
            offset = 0
          },
      code = 0,
      commentDepth = 0
    }

newtype Parser a = Parser
  { runParser :: ParserState -> (ParserState, Either Error a)
  }

instance Functor Parser where
  fmap f p = Parser $ \s ->
    case runParser p s of
      (s', Left e) -> (s', Left e)
      (s', Right a) -> (s', Right (f a))

instance Applicative Parser where
  pure a = Parser $ \s -> (s, Right a)

  pf <*> p = Parser $ \s ->
    case runParser pf s of
      (s', Left e) -> (s', Left e)
      (s', Right f) ->
        case runParser p s' of
          (s'', Left e) -> (s'', Left e)
          (s'', Right a) -> (s'', Right (f a))

instance Monad Parser where
  p >>= f = Parser $ \s ->
    case runParser p s of
      (s', Left e) -> (s', Left e)
      (s', Right a) -> runParser (f a) s'

parse :: Parser a -> T.Text -> Either Error a
parse p t = snd $ runParser p (initialState t)

getInput :: Parser AlexInput
getInput = Parser $ \s -> (s, Right $ input s)

setInput :: AlexInput -> Parser ()
setInput input = Parser $ \s -> (s {input}, Right ())

getCode :: Parser Int
getCode = Parser $ \s -> (s, Right $ code s)

setCode :: Int -> Parser ()
setCode code = Parser $ \s -> (s {code}, Right ())

getCommentDepth :: Parser Int
getCommentDepth = Parser $ \s -> (s, Right $ commentDepth s)

setCommentDepth :: Int -> Parser ()
setCommentDepth commentDepth = Parser $ \s -> (s {commentDepth}, Right ())

throw :: Error -> Parser a
throw err = Parser $ \s -> (s, Left err)
