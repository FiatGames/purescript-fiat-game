module FiatGame.WebSocket where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Maybe (Maybe(..))
import Control.Coroutine.Aff as CRA
import DOM.Websocket.Event.EventTypes as WSET

wsProducer
  :: forall eff
   . WS.WebSocket
  -> CR.Producer String (Aff (avar :: AVAR, exception :: EXCEPTION, dom :: DOM | eff)) Unit
wsProducer socket = CRA.produce \emit ->
  EET.addEventListener
    WSET.onMessage
    (listener emit)
    false
    (WS.socketToEventTarget socket)
  where
  listener emit = EET.eventListener \ev -> do
    for_ (readHelper WS.readMessageEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
        emit (Left msg)
  readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
  readHelper read =
    either (const Nothing) Just <<< runExcept <<< read <<< toForeign













