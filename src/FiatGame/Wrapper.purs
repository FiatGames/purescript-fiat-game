module FiatGame.Wrapper (makeWrapper, QueryParams(..)) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (ExceptT(..), lift, mapExceptT, runExceptT)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlDocumentToDocument, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement, getElementsByTagName)
import DOM.Node.Element (getAttribute)
import DOM.Node.HTMLCollection as C
import DOM.Node.Node (insertBefore, parentElement)
import DOM.Node.Types (elementToNode)
import Data.Bifunctor (bimap)
import Data.Either (Either, note)
import Data.Foldable (find)
import Data.Foreign (toForeign)
import Data.Int (fromString)
import Data.Lens ((^.))
import Data.Map (Map, fromFoldable)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.URI.AbsoluteURI (Query(..), _query, parse)

type QueryParams = Map String String

getQueryParams :: String -> Array String -> Either String QueryParams
getQueryParams href params = do
  parsed <- bimap show id $ parse href
  (Query q) <- note "No querystring in src" $ parsed ^. _query
  fromFoldable <$> traverse (getParam q) params
  where 
    getParam q param = do
      (Tuple _ mvalue) <- note ("No " <> param <> " attribute in querystring") $ find (\(Tuple h _) -> h == param) q
      value <- note ("No " <> param <> " attribute in querystring") mvalue
      pure $ Tuple param value

makeWrapper :: forall e. Array String -> Eff ( dom :: DOM | e) (Either String (Tuple HTMLElement QueryParams))
makeWrapper params = do
  htmlDoc <- window >>= document
  let doc = htmlDocumentToDocument htmlDoc
  scripts <- getElementsByTagName "script" doc
  l <- C.length scripts
  runExceptT $ do
    me <- ExceptT $ note "Can't find my script" <$> C.item (l-1) scripts
    src <- ExceptT $ note "Can't find src attribute" <$> getAttribute "src" me
    queryParams <- ExceptT $ pure $ getQueryParams src params
    let currScript = elementToNode me
    parEl <- ExceptT $ note "Can't find parent element" <$> parentElement currScript
    n <- lift $ do
      wrapper <- createElement "div" doc
      insertBefore (elementToNode wrapper) currScript (elementToNode parEl)
    el <- mapExceptT (pure <<< bimap show id <<< unwrap) $ readHTMLElement $ toForeign n
    pure $ Tuple el queryParams