module FiatGame.WebSocket where

import Prelude

import Chat.Types as Chat
import Control.Coroutine (Consumer)
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Control.Monad.Eff.Exception as E
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket (WebSocket)
import DOM.Websocket.WebSocket as WS
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode as Argonaut
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import FiatGame.ToClient.Types as ToClient
import FiatGame.ToServer.Types (Cmd(..))
import FiatGame.ToServer.Types as ToServer
import FiatGame.Types (FiatGameHash(..), FiatPlayer(..), FutureMove(..), GameState(..))
import Halogen as H

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

wsConsumer' :: forall f a m state settings move eff b. Monad m 
  => DecodeJson state 
  => DecodeJson settings
  => DecodeJson move
  => MonadEff (exception :: EXCEPTION | eff) m
  => Discard b 
  => (f Unit -> m b) -> (ToClient.Msg settings state move -> Unit -> f Unit) -> Consumer String m a
wsConsumer' query f = CR.consumer \msg -> do
  case jsonParser msg >>= decodeJson of
    Left err -> do
      _ <- liftEff $ throwException $ E.error err
      pure Nothing
    Right (g ::ToClient.Msg String String String) -> case g of
      (ToClient.Error {player,error}) -> do
        query $ H.action $ f $ ToClient.Error {player,error}
        pure Nothing
      (ToClient.Msg {hash, settings, state}) -> do
        case {settings':_, state':_} <$> fromJSONString settings <*> bar pGameState state of
          Left err -> do
            _ <- liftEff $ throwException $ E.error err
            pure Nothing
          Right {settings',state'} -> do
            query $ H.action $ f $ ToClient.Msg {hash,settings: settings', state: state'}
            pure Nothing

fromJSONString :: forall a. DecodeJson a => String -> Either String a
fromJSONString t = jsonParser t >>= Argonaut.decodeJson

bar :: forall f b a. Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
bar f s = maybe (pure Nothing) (map Just <<< f) s

pFmv :: forall a. DecodeJson a => FutureMove String -> Either String (FutureMove a)
pFmv (FutureMove mv) =  do
  m <- fromJSONString mv._futureMoveMove
  pure $ FutureMove mv{_futureMoveMove= m}

pGameState :: forall t219 t220. DecodeJson t220 => DecodeJson t219 => GameState String String -> Either String (GameState t220 t219)
pGameState (GameState s) = do
  st <- fromJSONString s._gameStateState
  fmv <- bar pFmv s._gameStateFutureMove
  pure $ GameState s{_gameStateState= st, _gameStateFutureMove= fmv}
      
wsConsumer :: forall f a m state settings move eff b. Monad m 
  => Generic state 
  => Generic settings
  => Generic move
  => MonadEff (exception :: EXCEPTION | eff) m
  => Discard b 
  => (f Unit -> m b) -> (ToClient.Msg settings state move -> Unit -> f Unit) -> Consumer String m a
wsConsumer query f = CR.consumer \msg -> do
  case jsonParser msg >>= decodeJson of
    Left err -> do
      _ <- liftEff $ throwException $ E.error err
      pure Nothing
    Right g -> do
      query $ H.action $ f g
      pure Nothing

wsChatConsumer :: forall f a m eff b. Monad m 
  => MonadEff (exception :: EXCEPTION | eff) m
  => Discard b 
  => (f Unit -> m b) -> (Chat.ToClient -> Unit -> f Unit) -> Consumer String m a
wsChatConsumer query f = CR.consumer \msg -> do
  case jsonParser msg >>= decodeJson of
    Left err -> do
      _ <- liftEff $ throwException $ E.error err
      pure Nothing
    Right g -> do
      query $ H.action $ f g
      pure Nothing

wsChatSender :: forall eff a m msg. Monad m 
  => MonadEff (dom :: DOM | eff ) m
  => Int -> WebSocket -> (msg -> Chat.ToServer) -> Consumer msg m a
wsChatSender userId socket f = CR.consumer \msg -> do
  let cmd = f msg
  liftEff $ WS.sendString socket $ stringify $ encodeJson cmd
  pure Nothing

wsSender :: forall eff settings move a m msg. Monad m 
  => MonadEff (dom :: DOM | eff ) m
  => Generic move 
  => Generic settings
  => Int -> WebSocket -> (msg -> (Tuple FiatGameHash (ToServer.Cmd settings move))) -> Consumer msg m a
wsSender userId socket f = CR.consumer \msg -> do
  let (Tuple hash cmd) = f msg
  liftEff $ WS.sendString socket $ stringify $ encodeJson $ ToServer.Msg
    { player: FiatPlayer userId
    , cmd
    , hash
    }
  pure Nothing

wsSender' :: forall eff settings move a m msg. Monad m 
  => MonadEff (dom :: DOM | eff ) m
  => EncodeJson move 
  => EncodeJson settings
  => Int -> WebSocket -> (msg -> (Tuple FiatGameHash (ToServer.Cmd settings move))) -> Consumer msg m a
wsSender' userId socket f = CR.consumer \msg -> do
  let (Tuple hash cmd) = f msg
  liftEff $ WS.sendString socket $ stringify $ encodeJson $ ToServer.Msg
    { player: FiatPlayer userId
    , cmd: pCmd cmd
    , hash: hash
    }
  pure Nothing

pCmd :: forall s m. EncodeJson s => EncodeJson m => ToServer.Cmd s m -> ToServer.Cmd String String
pCmd StartGame = StartGame
pCmd (UpdateSettings s) = UpdateSettings $ stringify $ Argonaut.encodeJson s
pCmd (MakeMove mv) = MakeMove $ stringify $ Argonaut.encodeJson mv