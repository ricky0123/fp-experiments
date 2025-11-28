module Main where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay, error, joinFiber, launchAff, launchAff_, makeAff, message, nonCanceler)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (debug)
import Effect.Console (log)
import Effect.Exception (Error, throw, try)
import Effect.Ref as Ref
import Node.ChildProcess (exitH, stderr, stdout)
import Node.ChildProcess as ChildProcess
import Node.ChildProcess.Types (Exit(..), customShell)
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
import Node.Stream (dataHStr, setEncoding)
import Promise.Aff (Promise, toAffE)


main :: Effect Unit
main = launchAff_ do
  -- Simple Aff success
  execAffSuccess >>= debug
  debug "Experiment 1 done"

  -- Simple Aff from Promise
  simpleSuccessAffFromPromise >>= debug
  debug "Experiment 2 done"

  -- Aff from makeAff that throws error by calling `done $ Left $ error "blah blah"`
  attempt simpleAffError >>= debug <<< show <<< lmap (String.take 5) <<< lmap message
  debug "Experiment 3 done"

  -- Aff from makeAff that throws error by using "throw"
  attempt affErrorFromThrow >>= debug <<< show <<< lmap message
  debug "Experiment 4 done"

  -- use "try" within makeAff
  attempt tryWithinMakeAff >>= debug <<< show <<< lmap message
  debug "Experiment 5 done"

  -- launch nonexistent system command and propagate error correctly
  attempt nonExistentCommand >>= debug <<< show <<< lmap message
  debug "Experiment 6 done"

  -- same but from promise
  attempt nonExistentCommandFromPromise >>= debug <<< show <<< lmap message
  debug "Experiment 7 done"

  debug "Done"

foreign import _spawnAsync :: Effect (Promise String)

nonExistentCommandFromPromise :: Aff String
nonExistentCommandFromPromise = toAffE _spawnAsync


nonExistentCommand :: Aff String
nonExistentCommand = makeAff \done -> do
  p <- ChildProcess.spawn' "doesnt exist" []
        $ _ { shell = Just $ customShell "/bin/bash"}
  
  err <- Ref.new ""
  out <- Ref.new ""

  let
    stdoutStream = stdout p
    stderrStream = stderr p

  setEncoding stdoutStream UTF8
  setEncoding stderrStream UTF8

  stdoutStream # on_ dataHStr \s -> 
    Ref.modify_ (\curr -> curr <> s) out

  stderrStream # on_ dataHStr \s ->
    Ref.modify_ (\curr -> curr <> s) err  

  p # on_ exitH \ev -> do
    case ev of
      Normally i -> 
        if (i == 0)
          then done <<< Right =<< Ref.read out
          else done <<< Left <<< error =<< Ref.read err
      _ -> done <<< Left <<< error =<< Ref.read err

  pure nonCanceler


--| this one is confusing
--| `try` catches exception but then passing it to `done` throws it again
tryWithinMakeAff :: Aff String
tryWithinMakeAff = makeAff \done -> do
  res <- try do
    _ <- throw "threw error in tryWithinMakeAff"
    pure "success"
  done $ res
  pure nonCanceler


affErrorFromThrow :: Aff String
affErrorFromThrow = makeAff \done -> do
  _ <- throw "threw error in affErrorFromThrow"
  done $ Right "Success"
  pure nonCanceler


simpleAffError :: Aff String
simpleAffError = makeAff \done -> do
  done $ Left $ error "fucked up"
  pure nonCanceler

execAffSuccess :: Aff String
execAffSuccess = makeAff \done -> do
  log "Starting experiment 1"
  p <- ChildProcess.spawn' "echo" ["hello world"] $ _ { shell = Just $ customShell "/bin/bash"}
  p #  on_ exitH \ev -> do
    case ev of
      Normally i ->
        if (i == 0)
          then done (Right "okay")
          else done (Left $ error "non zero exit code")
      _ -> done (Left $ error "bad exit")
  pure nonCanceler

foreign import _simpleSuccessPromise :: Effect (Promise String)

simpleSuccessAffFromPromise :: Aff String
simpleSuccessAffFromPromise = toAffE _simpleSuccessPromise
