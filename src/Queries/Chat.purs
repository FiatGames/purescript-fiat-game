-- File auto generated by purescript-bridge! --
module Queries.Chat where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (Array, Int, String)

import Prelude
import Data.Generic (class Generic, gCompare, gEq, gShow)

newtype ChatCreate =
    ChatCreate {
      _chatCreateGameId :: Int
    , _chatCreateUsers :: Array Int
    , _chatCreateName :: String
    , _chatCreateInitialMsg :: String
    }

derive instance genericChatCreate :: Generic ChatCreate

instance showChatCreate :: Show ChatCreate where
  show = gShow
instance eqChatCreate :: Eq ChatCreate where
  eq = gEq
instance ordChatCreate :: Ord ChatCreate where
  compare = gCompare

derive instance newtypeChatCreate :: Newtype ChatCreate _


--------------------------------------------------------------------------------
_ChatCreate :: Iso' ChatCreate { _chatCreateGameId :: Int, _chatCreateUsers :: Array Int, _chatCreateName :: String, _chatCreateInitialMsg :: String}
_ChatCreate = _Newtype

chatCreateGameId :: Lens' ChatCreate Int
chatCreateGameId = _Newtype <<< prop (SProxy :: SProxy "_chatCreateGameId")

chatCreateUsers :: Lens' ChatCreate (Array Int)
chatCreateUsers = _Newtype <<< prop (SProxy :: SProxy "_chatCreateUsers")

chatCreateName :: Lens' ChatCreate String
chatCreateName = _Newtype <<< prop (SProxy :: SProxy "_chatCreateName")

chatCreateInitialMsg :: Lens' ChatCreate String
chatCreateInitialMsg = _Newtype <<< prop (SProxy :: SProxy "_chatCreateInitialMsg")

--------------------------------------------------------------------------------