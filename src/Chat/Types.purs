-- File auto generated by purescript-bridge! --
module Chat.Types where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import FiatGame.Types (FiatPlayer)
import Model (ChatMessage)
import Prim (Array, Int, String)

import Prelude
import Data.Generic (class Generic, gCompare, gEq, gShow)

data ToServerCmd =
    PostMessage Int String

derive instance genericToServerCmd :: Generic ToServerCmd

instance showToServerCmd :: Show ToServerCmd where
  show = gShow
instance eqToServerCmd :: Eq ToServerCmd where
  eq = gEq
instance ordToServerCmd :: Ord ToServerCmd where
  compare = gCompare


--------------------------------------------------------------------------------
_PostMessage :: Prism' ToServerCmd { a :: Int, b :: String }
_PostMessage = prism' (\{ a, b } -> PostMessage a b) f
  where
    f (PostMessage a b) = Just $ { a: a, b: b }

--------------------------------------------------------------------------------
newtype ToServer =
    ToServer {
      player :: FiatPlayer
    , cmd :: ToServerCmd
    }

derive instance genericToServer :: Generic ToServer

instance showToServer :: Show ToServer where
  show = gShow
instance eqToServer :: Eq ToServer where
  eq = gEq
instance ordToServer :: Ord ToServer where
  compare = gCompare

derive instance newtypeToServer :: Newtype ToServer _


--------------------------------------------------------------------------------
_ToServer :: Iso' ToServer { player :: FiatPlayer, cmd :: ToServerCmd}
_ToServer = _Newtype

--------------------------------------------------------------------------------
newtype ToClient =
    Messages (Array ChatMessage)

derive instance genericToClient :: Generic ToClient

instance showToClient :: Show ToClient where
  show = gShow
instance eqToClient :: Eq ToClient where
  eq = gEq
instance ordToClient :: Ord ToClient where
  compare = gCompare

derive instance newtypeToClient :: Newtype ToClient _


--------------------------------------------------------------------------------
_Messages :: Iso' ToClient (Array ChatMessage)
_Messages = _Newtype
--------------------------------------------------------------------------------
