module Main where

import Scala.Foldable (Foldable, fold, foldMap, length, listFoldable, product, sum)
import Scala.Monoid (addition, additionMonoid, intSemiring, listMonoid, multiplication)
import Prelude hiding (Foldable (..), Monoid (..), Semigroup (..), product, sum, (*), (+))

main :: IO ()
main = do
  print $ sum [1 .. 5]
  print $ product [1 .. 5]
  print $ fold [[1], [2], [3], [4], [5]]
  print $ let ?monoid = addition in fold [5 .. 10]
  print $ let ?monoid = multiplication in fold [5 .. 10]
  print $ totalLength [[1..4], [4..16], [3..8]]
  where
    ?semiring = intSemiring
    ?foldable = listFoldable
    ?monoid = listMonoid

totalLength :: (?foldable :: Foldable t) => t (t a) -> Int
totalLength = foldMap length
  where
    ?monoid = additionMonoid
