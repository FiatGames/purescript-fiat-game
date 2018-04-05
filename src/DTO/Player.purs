-- File auto generated by purescript-bridge! --
module DTO.Player where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (Int, String)

import Prelude
import Data.Generic (class Generic, gCompare, gEq, gShow)

newtype Player =
    Player {
      id :: Int
    , name :: String
    }

derive instance genericPlayer :: Generic Player

instance showPlayer :: Show Player where
  show = gShow
instance eqPlayer :: Eq Player where
  eq = gEq
instance ordPlayer :: Ord Player where
  compare = gCompare

derive instance newtypePlayer :: Newtype Player _


--------------------------------------------------------------------------------
_Player :: Iso' Player { id :: Int, name :: String}
_Player = _Newtype

--------------------------------------------------------------------------------