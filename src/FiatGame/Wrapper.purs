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
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.URI.AbsoluteURI (Query(..), _query, parse)

data QueryParams = QueryParams
  { webSocketUri :: String
  , userId :: Int 
  }

getQueryParams :: String -> Either String QueryParams
getQueryParams href = do
  parsed <- bimap show id $ parse href
  (Query q) <- note "No querystring in src" $ parsed ^. _query
  (Tuple _ msocket) <- note "No socket attribute in querystring" $ find (\(Tuple h _) -> h == "socket") q
  socket <- note "No socket attribute in querystring" msocket
  (Tuple _ muserId) <- note "No socket attribute in querystring" $ find (\(Tuple h _) -> h == "userId") q
  userId <- note "Can't parse userId" (join (fromString <$> muserId))
  pure $ QueryParams
    { webSocketUri : socket
    , userId : userId
    }

makeWrapper :: forall e. Eff ( dom :: DOM | e) (Either String (Tuple HTMLElement QueryParams))
makeWrapper = do
  htmlDoc <- window >>= document
  let doc = htmlDocumentToDocument htmlDoc
  scripts <- getElementsByTagName "script" doc
  l <- C.length scripts
  runExceptT $ do
    me <- ExceptT $ note "Can't find my script" <$> C.item (l-1) scripts
    src <- ExceptT $ note "Can't find src attribute" <$> getAttribute "src" me
    queryParams <- ExceptT $ pure $ getQueryParams src
    let currScript = elementToNode me
    parEl <- ExceptT $ note "Can't find parent element" <$> parentElement currScript
    n <- lift $ do
      wrapper <- createElement "div" doc
      insertBefore (elementToNode wrapper) currScript (elementToNode parEl)
    el <- mapExceptT (pure <<< bimap show id <<< unwrap) $ readHTMLElement $ toForeign n
    pure $ Tuple el queryParams