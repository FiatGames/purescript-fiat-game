-- File auto generated by purescript-bridge! --
module DTO.GameDetail where

import DTO.Player (Player)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (Array)

import Prelude
import Data.Generic (class Generic, gCompare, gEq, gShow)

newtype GameDetail =
    GameDetail {
      players :: Array Player
    }

derive instance genericGameDetail :: Generic GameDetail

instance showGameDetail :: Show GameDetail where
  show = gShow
instance eqGameDetail :: Eq GameDetail where
  eq = gEq
instance ordGameDetail :: Ord GameDetail where
  compare = gCompare

derive instance newtypeGameDetail :: Newtype GameDetail _


--------------------------------------------------------------------------------
_GameDetail :: Iso' GameDetail { players :: Array Player}
_GameDetail = _Newtype

--------------------------------------------------------------------------------