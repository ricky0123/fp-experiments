module Main where

import Prelude

import Control.Monad.Free
import Effect.Random (random)

import Effect (Effect)
import Effect.Console (log)

data Prob a = BernoulliTrial Number (Boolean -> a) | Gaussian Number Number (Number -> a)
derive instance functorProb :: Functor Prob

type ProbM = Free Prob

bernoulliTrial :: Number -> ProbM Boolean
bernoulliTrial p = liftF (BernoulliTrial p identity)

gaussian :: Number -> Number -> ProbM Number
gaussian mean stddev = liftF (Gaussian mean stddev identity)

interpretProb :: forall a. Prob a -> Effect a
interpretProb (BernoulliTrial prob next) = do
  sample <- random
  pure $ next $ sample < prob
interpretProb (Gaussian mean stddev next) = do
  pure $ next mean  -- placeholder for actual sampling

runProb :: forall a. ProbM a -> Effect a
runProb = foldFree interpretProb

-- define a mixture of Gaussians
dist = do
  flipResult <- bernoulliTrial 0.3
  if flipResult
    then gaussian 5.0 3.0
    else gaussian (-4.0) 2.0

main :: Effect Unit
main = do
  sample <- runProb dist
  log $ show sample
