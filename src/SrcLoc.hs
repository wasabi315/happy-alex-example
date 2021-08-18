{-# LANGUAGE DeriveTraversable #-}

module SrcLoc where

data Span = Span
  { start :: Int,
    end :: Int
  }
  deriving (Eq, Show)

mergeSpan :: Span -> Span -> Span
mergeSpan (Span s1 e1) (Span s2 e2) = Span (s1 `min` s2) (e1 `max` e2)

data Located a = At Span a
  deriving (Eq, Show, Functor, Foldable, Traversable)

loc :: Located a -> Span
loc (At sp _) = sp

val :: Located a -> a
val (At _ a) = a
