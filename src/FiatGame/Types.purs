-- File auto generated by purescript-bridge! --
module FiatGame.Types where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (Int, String)

import Prelude
import Data.Generic (class Generic, gCompare, gEq, gShow)

data GameStage =
    SettingUp
  | Playing
  | Done

derive instance genericGameStage :: Generic GameStage

instance showGameStage :: Show GameStage where
  show = gShow
instance eqGameStage :: Eq GameStage where
  eq = gEq
instance ordGameStage :: Ord GameStage where
  compare = gCompare


--------------------------------------------------------------------------------
_SettingUp :: Prism' GameStage Unit
_SettingUp = prism' (\_ -> SettingUp) f
  where
    f SettingUp = Just unit
    f _ = Nothing

_Playing :: Prism' GameStage Unit
_Playing = prism' (\_ -> Playing) f
  where
    f Playing = Just unit
    f _ = Nothing

_Done :: Prism' GameStage Unit
_Done = prism' (\_ -> Done) f
  where
    f Done = Just unit
    f _ = Nothing

--------------------------------------------------------------------------------
newtype FutureMove a =
    FutureMove {
      _futureMoveTime :: String
    , _futureMoveMove :: a
    }

derive instance genericFutureMove :: Generic a => Generic (FutureMove a)

instance showFutureMove :: Generic a => Show (FutureMove a) where
  show = gShow
instance eqFutureMove :: Generic a => Eq (FutureMove a) where
  eq = gEq
instance ordFutureMove :: Generic a => Ord (FutureMove a) where
  compare = gCompare

derive instance newtypeFutureMove :: Newtype (FutureMove a) _


--------------------------------------------------------------------------------
_FutureMove :: forall a. Iso' (FutureMove a) { _futureMoveTime :: String, _futureMoveMove :: a}
_FutureMove = _Newtype

futureMoveTime :: forall a. Lens' (FutureMove a) String
futureMoveTime = _Newtype <<< prop (SProxy :: SProxy "_futureMoveTime")

futureMoveMove :: forall a. Lens' (FutureMove a) a
futureMoveMove = _Newtype <<< prop (SProxy :: SProxy "_futureMoveMove")

--------------------------------------------------------------------------------
data FiatPlayer =
    FiatPlayer Int
  | System

derive instance genericFiatPlayer :: Generic FiatPlayer

instance showFiatPlayer :: Show FiatPlayer where
  show = gShow
instance eqFiatPlayer :: Eq FiatPlayer where
  eq = gEq
instance ordFiatPlayer :: Ord FiatPlayer where
  compare = gCompare


--------------------------------------------------------------------------------
_FiatPlayer :: Prism' FiatPlayer Int
_FiatPlayer = prism' FiatPlayer f
  where
    f (FiatPlayer a) = Just $ a
    f _ = Nothing

_System :: Prism' FiatPlayer Unit
_System = prism' (\_ -> System) f
  where
    f System = Just unit
    f _ = Nothing

--------------------------------------------------------------------------------
newtype GameState a b =
    GameState {
      _gameStateStage :: GameStage
    , _gameStateState :: a
    , _gameStateFutureMove :: Maybe (FutureMove b)
    }

derive instance genericGameState :: (Generic a, Generic b) => Generic (GameState a b)

instance showGameState :: (Generic a, Generic b) => Show (GameState a b) where
  show = gShow
instance eqGameState :: (Generic a, Generic b) => Eq (GameState a b) where
  eq = gEq
instance ordGameState :: (Generic a, Generic b) => Ord (GameState a b) where
  compare = gCompare

derive instance newtypeGameState :: Newtype (GameState a b) _


--------------------------------------------------------------------------------
_GameState :: forall a b. Iso' (GameState a b) { _gameStateStage :: GameStage, _gameStateState :: a, _gameStateFutureMove :: Maybe (FutureMove b)}
_GameState = _Newtype

gameStateStage :: forall a b. Lens' (GameState a b) GameStage
gameStateStage = _Newtype <<< prop (SProxy :: SProxy "_gameStateStage")

gameStateState :: forall a b. Lens' (GameState a b) a
gameStateState = _Newtype <<< prop (SProxy :: SProxy "_gameStateState")

gameStateFutureMove :: forall a b. Lens' (GameState a b) (Maybe (FutureMove b))
gameStateFutureMove = _Newtype <<< prop (SProxy :: SProxy "_gameStateFutureMove")

--------------------------------------------------------------------------------
newtype FiatGameHash =
    FiatGameHash String

derive instance genericFiatGameHash :: Generic FiatGameHash

instance showFiatGameHash :: Show FiatGameHash where
  show = gShow
instance eqFiatGameHash :: Eq FiatGameHash where
  eq = gEq
instance ordFiatGameHash :: Ord FiatGameHash where
  compare = gCompare

derive instance newtypeFiatGameHash :: Newtype FiatGameHash _


--------------------------------------------------------------------------------
_FiatGameHash :: Iso' FiatGameHash String
_FiatGameHash = _Newtype
--------------------------------------------------------------------------------
