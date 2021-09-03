module Scala.Monoid where

import Data.Kind (Type)
import Prelude hiding (Foldable (..), Monoid (..), Semigroup (..), product, sum, (*), (+))
import Prelude qualified

newtype Semigroup a = Semigroup
  { _sappend :: a -> a -> a
  }

sappend :: (?semigroup :: Semigroup a) => a -> a -> a
sappend = _sappend ?semigroup

data Monoid a = Monoid
  { _semigroup :: Semigroup a,
    _mempty :: a
  }

(<>) :: (?monoid :: Monoid a) => a -> a -> a
(<>) = sappend
  where
    ?semigroup = _semigroup ?monoid

mempty :: (?monoid :: Monoid a) => a
mempty = _mempty ?monoid

data Semiring a = Semiring
  { _addition :: Monoid a,
    _multiplication :: Monoid a
  }

addition :: (?semiring :: Semiring a) => Monoid a
addition = _addition ?semiring

multiplication :: (?semiring :: Semiring a) => Monoid a
multiplication = _multiplication ?semiring

(+) :: (?semiring :: Semiring a) => a -> a -> a
(+) = (<>)
  where
    ?monoid = addition

(*) :: (?semiring :: Semiring a) => a -> a -> a
(*) = (<>)
  where
    ?monoid = multiplication

listSemigroup :: Semigroup [a]
listSemigroup = Semigroup (++)

listMonoid :: Monoid [a]
listMonoid = Monoid listSemigroup []

additionSemigroup :: Semigroup Int
additionSemigroup = Semigroup (Prelude.+)

additionMonoid :: Monoid Int
additionMonoid = Monoid additionSemigroup 0

multiplicationSemigroup :: Semigroup Int
multiplicationSemigroup = Semigroup (Prelude.*)

multiplicationMonoid :: Monoid Int
multiplicationMonoid = Monoid multiplicationSemigroup 1

intSemiring :: Semiring Int
intSemiring = Semiring additionMonoid multiplicationMonoid
