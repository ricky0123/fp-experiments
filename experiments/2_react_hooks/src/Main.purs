module Main where

import Prelude

import Control.Monad.Rec.Class (untilJust)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Halogen.Subscription as HS
import Partial.Unsafe (unsafeCrashWith)
import React.Basic.DOM as R
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.Events as Events
import React.Basic.Hooks (type (/\), (/\), type (&))
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document, history)

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> unsafeCrashWith "Container element not found."
    Just c  -> do
      root <- createRoot c
      app <- mkCounter
      renderRoot root $ app unit


type UseSignal hooks
  = React.UseState Boolean hooks

useSignal :: React.Hook UseSignal (Boolean /\ Effect Unit)
useSignal = React.do
  signal /\ setSignalValue <- React.useState false
  let
    triggerSignal :: Effect Unit
    triggerSignal = setSignalValue \current -> not current
  pure $ signal /\ triggerSignal

type UseMyData hooks
  = ( hooks
    & React.UseState (Array Int)
    & React.UseState (Maybe (HS.SubscribeIO Int))
    & UseSignal
    & UseSignal
    & React.UseEffect (Array Unit)
    & React.UseEffect (Array Boolean)
    )

useMyData :: React.Hook UseMyData (Array Int /\ (Effect Unit) /\ (Effect Unit) /\ (Effect Unit))
useMyData = React.do
  history /\ setHistory <- React.useState ([] :: Array Int)
  subIO /\ setSubIO <- React.useState (Nothing :: Maybe (HS.SubscribeIO Int))
  pauseSignal /\ pause <- useSignal
  playSignal /\ play <- useSignal

  React.useEffect [] do
    (subIO' :: HS.SubscribeIO Int) <- HS.create
    setSubIO \_ -> Just subIO'
    sub <- HS.subscribe subIO'.emitter \i -> setHistory (_ <> [i])

    pure do
      HS.unsubscribe sub
      pure unit

  React.useEffect [pauseSignal] do
    pure do
      pure unit
  
  let
    notify :: Effect Unit
    notify = case subIO of
      Nothing -> pure unit
      Just subIO' -> do
        HS.notify subIO'.listener 1

  pure $ history /\ pause /\ play /\ notify


mkCounter :: React.Component Unit
mkCounter = do
  React.component "Counter" \_ -> React.do
    history /\ pause /\ play /\ notify <- useMyData

    pure
      $ R.div_
          [ R.div_
              [ R.text $ "Increment: " <> show history ]
          , R.button
              { onClick: Events.handler_ do
                  pause
              , children:
                  [ R.text "Pause" ]
              }
          , R.button
              { onClick: Events.handler_ do
                  play
              , children:
                  [ R.text "Play" ]
              }
          , R.button
              { onClick: Events.handler_ do
                  notify
              , children:
                  [ R.text "Notify" ]
              }
          ]
