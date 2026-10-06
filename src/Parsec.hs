{-# LANGUAGE LambdaCase #-}

module Parsec where

import Control.Applicative (Alternative (..))
import Data.List (nub)

data Error i e
  = EndOfInput
  | Unexpected i
  | CustomError e
  | Empty
  deriving (Eq, Show)

newtype Parser i e a = Parser {runParser :: [i] -> Either [Error i e] (a, [i])}

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \case
  [] -> Left [EndOfInput]
  (x : xs)
    | predicate x -> Right (x, xs)
    | otherwise -> Left [Unexpected x]

char :: (Eq i) => i -> Parser i e i
char i = satisfy (== i)

instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input -> do
    (out, rest) <- p input
    pure (f out, rest)

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  (Parser f) <*> (Parser p) = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (out, rest) <- p input
    runParser (k out) rest

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)
