module FiatGame.WebSocket where

import Prelude

import Control.Coroutine (Await(..), Consumer)
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Control.Monad.Eff.Exception as E
import Control.Monad.Except (runExcept)
import Control.Monad.Free.Trans (FreeT)
import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket (WebSocket)
import DOM.Websocket.WebSocket as WS
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

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


wsConsumer :: forall a m move eff. Monad m 
  => Generic move 
  => MonadEff ( exception :: EXCEPTION | eff) m
  => (move -> m (Maybe a)) -> Consumer String m a
wsConsumer f = CR.consumer \msg -> do
  case jsonParser msg >>= decodeJson of
    Left err -> do
      _ <- liftEff $ throwException $ E.error err
      pure Nothing
    Right enc -> f enc

wsSender :: forall eff move a m msg. Monad m 
  => MonadEff (dom :: DOM | eff ) m
  => Generic move 
  => Int -> WebSocket -> (msg -> move) -> Consumer msg m a
wsSender userId socket f = CR.consumer \msg -> do
  liftEff $ WS.sendString socket (stringify $ encodeJson (Tuple userId (f msg)))
  pure Nothing












