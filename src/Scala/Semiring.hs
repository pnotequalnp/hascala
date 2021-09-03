module Scala.Semiring where

import Scala.Monoid
import Prelude hiding (Monoid (..), Semigroup (..), (*), (+))

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

zero :: (?semiring :: Semiring a) => a
zero = mempty
  where
    ?monoid = addition

one :: (?semiring :: Semiring a) => a
one = mempty
  where
    ?monoid = multiplication

intSemiring :: Semiring Int
intSemiring = Semiring intMonoidAddition intMonoidMultiplication
