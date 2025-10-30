module Main where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Console (log)
import Web.DOM.Element (Element, toEventTarget, toNode)
import Web.DOM.Node (setTextContent, textContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (click)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  parentNode <- toNonElementParentNode <$> (window >>= document)
  maybeButton <- getElementById "increment_button" parentNode
  maybeDisplayDiv <- getElementById "display" parentNode
  case maybeButton, maybeDisplayDiv of
    Just button, Just displayDiv -> do
      listener <- eventListener $ pure $ incrementCounter displayDiv
      addEventListener click listener true (toEventTarget button)
      log "Counter initialized"
    _, _ -> log "Failed to initialize counter"

incrementCounter :: Element -> Effect Unit
incrementCounter divElement = do
  node <- pure $ toNode divElement
  currentInt <- maybe 0 identity <$> (Int.fromString <$> textContent node)
  setTextContent (show $ currentInt + 1) node
