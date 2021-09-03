module Scala.Monoid where

import Scala.Semigroup
import Prelude hiding (Monoid (..), Semigroup (..))

data Monoid a = Monoid
  { _semigroup :: Semigroup a,
    _mempty :: a
  }

semigroup :: (?monoid :: Monoid a) => Semigroup a
semigroup = _semigroup ?monoid

(<>) :: (?monoid :: Monoid a) => a -> a -> a
(<>) = sappend
  where
    ?semigroup = semigroup

mempty :: (?monoid :: Monoid a) => a
mempty = _mempty ?monoid

listMonoid :: Monoid [a]
listMonoid = Monoid listSemigroup []

intMonoidAddition :: Monoid Int
intMonoidAddition = Monoid intSemigroupAddition 0

intMonoidMultiplication :: Monoid Int
intMonoidMultiplication = Monoid intSemigroupMultiplication 1
