-- File auto generated by purescript-bridge! --
module GameType where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))

import Prelude
import Data.Generic (class Generic, gCompare, gEq, gShow)

data GameType =
    TicTacToe
  | Tak

derive instance genericGameType :: Generic GameType

instance showGameType :: Show GameType where
  show = gShow
instance eqGameType :: Eq GameType where
  eq = gEq
instance ordGameType :: Ord GameType where
  compare = gCompare


--------------------------------------------------------------------------------
_TicTacToe :: Prism' GameType Unit
_TicTacToe = prism' (\_ -> TicTacToe) f
  where
    f TicTacToe = Just unit
    f _ = Nothing

_Tak :: Prism' GameType Unit
_Tak = prism' (\_ -> Tak) f
  where
    f Tak = Just unit
    f _ = Nothing

--------------------------------------------------------------------------------