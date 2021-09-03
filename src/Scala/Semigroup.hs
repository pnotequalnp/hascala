module Scala.Semigroup where

import Prelude hiding (Semigroup (..))

newtype Semigroup a = Semigroup
  { _sappend :: a -> a -> a
  }

sappend :: (?semigroup :: Semigroup a) => a -> a -> a
sappend = _sappend ?semigroup

intSemigroupAddition :: Semigroup Int
intSemigroupAddition = Semigroup (+)

intSemigroupMultiplication :: Semigroup Int
intSemigroupMultiplication = Semigroup (*)

listSemigroup :: Semigroup [a]
listSemigroup = Semigroup (++)
