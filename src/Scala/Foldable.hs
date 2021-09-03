module Scala.Foldable where

import Data.Kind (Type)
import Scala.Monoid
import Prelude hiding (Foldable (..), Monoid (..), Semigroup (..), product, sum, (*), (+))

newtype Foldable (t :: Type -> Type) = Foldable
  { foldMap' :: forall a b. (?monoid :: Monoid b) => (a -> b) -> t a -> b
  }

foldMap ::
  forall a b (t :: Type -> Type).
  ( ?foldable :: Foldable t,
    ?monoid :: Monoid b
  ) =>
  (a -> b) ->
  t a ->
  b
foldMap = foldMap' ?foldable

fold ::
  forall a (t :: Type -> Type).
  ( ?foldable :: Foldable t,
    ?monoid :: Monoid a
  ) =>
  t a ->
  a
fold = foldMap id

sum ::
  forall a (t :: Type -> Type).
  ( ?foldable :: Foldable t,
    ?semiring :: Semiring a
  ) =>
  t a ->
  a
sum = fold
  where
    ?monoid = addition

product ::
  forall a (t :: Type -> Type).
  ( ?foldable :: Foldable t,
    ?semiring :: Semiring a
  ) =>
  t a ->
  a
product = fold
  where
    ?monoid = multiplication

length :: forall a (t :: Type -> Type). (?foldable :: Foldable t) => t a -> Int
length = foldMap $ const 1
  where
    ?monoid = additionMonoid

listFoldable :: Foldable []
listFoldable = Foldable go
  where
    go :: forall a b. (?monoid :: Monoid b) => (a -> b) -> [a] -> b
    go f = \case
      [] -> mempty
      x : xs -> f x <> go f xs

maybeFoldable :: Foldable Maybe
maybeFoldable = Foldable $ maybe mempty
