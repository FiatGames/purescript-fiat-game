-- File auto generated by purescript-bridge! --
module Model where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import FiatGame.GameState (GameStage)
import GameType (GameType)
import Prim (Int, String)

import Prelude
import Data.Generic (class Generic)

newtype User =
    User {
      userIdent :: String
    , userPassword :: Maybe String
    }

derive instance genericUser :: Generic User

derive instance newtypeUser :: Newtype User _


--------------------------------------------------------------------------------
_User :: Iso' User { userIdent :: String, userPassword :: Maybe String}
_User = _Newtype

--------------------------------------------------------------------------------
newtype Game =
    Game {
      gameName :: String
    , gameType :: GameType
    , gameCode :: String
    , gameStage :: GameStage
    , gameSettings :: String
    , gameState :: Maybe String
    , gameUserId :: Int
    }

derive instance genericGame :: Generic Game

derive instance newtypeGame :: Newtype Game _


--------------------------------------------------------------------------------
_Game :: Iso' Game { gameName :: String, gameType :: GameType, gameCode :: String, gameStage :: GameStage, gameSettings :: String, gameState :: Maybe String, gameUserId :: Int}
_Game = _Newtype

--------------------------------------------------------------------------------
newtype ChatRoom =
    ChatRoom {
      chatRoomName :: String
    , chatRoomGameId :: Maybe Int
    }

derive instance genericChatRoom :: Generic ChatRoom

derive instance newtypeChatRoom :: Newtype ChatRoom _


--------------------------------------------------------------------------------
_ChatRoom :: Iso' ChatRoom { chatRoomName :: String, chatRoomGameId :: Maybe Int}
_ChatRoom = _Newtype

--------------------------------------------------------------------------------
newtype ChatMessage =
    ChatMessage {
      chatMessageMessage :: String
    , chatMessageSent :: String
    , chatMessageUserId :: Int
    , chatMessageChatRoomId :: Int
    }

derive instance genericChatMessage :: Generic ChatMessage

derive instance newtypeChatMessage :: Newtype ChatMessage _


--------------------------------------------------------------------------------
_ChatMessage :: Iso' ChatMessage { chatMessageMessage :: String, chatMessageSent :: String, chatMessageUserId :: Int, chatMessageChatRoomId :: Int}
_ChatMessage = _Newtype

--------------------------------------------------------------------------------
