module Ranks
  ( initWeights,
    ilsrPairwise,
  )
where

import Data.List ((!!), iterate)
import Import hiding ((<>), Vector, fromList)
import Numeric.LinearAlgebra

mean :: Vector R -> R
mean v = sumElements v / fromIntegral (size v)

expTransform :: Vector R -> Vector R
expTransform weights = weights' / scalar (mean weights')
  where
    weights' = exp (weights - scalar (mean weights))

logTransform :: Vector R -> Vector R
logTransform weights = params - scalar (mean params)
  where
    params = log weights

initWeights :: Int -> Vector R
initWeights = konst 1

iterLsr :: Vector R -> Vector R
iterLsr = expTransform

initChain :: Int -> R -> Matrix R
initChain n alpha = scalar alpha * konst 1 (n, n)

lsrPairwise :: Matrix R -> R -> Vector R -> Vector R
lsrPairwise bm alpha weights = logTransform (statdist chain)
  where
    n = size weights
    denom = asRow weights
    chain' = initChain n alpha + tr' bm / (denom + tr' denom)
    chain = chain' - (diagl . fmap sumElements . toRows) chain'

ilsrPairwise :: Matrix R -> R -> Vector R
ilsrPairwise bm alpha = iterate (lsr . iterLsr) (lsr weights) !! 100
  where
    weights = initWeights (rows bm)
    lsr = lsrPairwise bm alpha

statdist :: Matrix R -> Vector R
statdist chain = res / scalar (mean res)
  where
    (_, u, _, _) = lu (tr' chain)
    left = u ?? (DropLast 1, DropLast 1)
    right = negate (u ?? (DropLast 1, TakeLast 1))
    res = vjoin [flatten (triSolve Upper left right), 1.0]
