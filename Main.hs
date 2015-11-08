module Main (
    main
) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.Conduit
import Data.Maybe (fromJust)
import GHCJS.DOM
       (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getBody, createElement, createTextNode, click)
import GHCJS.DOM.Element (Element, setInnerHTML)
import GHCJS.DOM.Node (appendChild, removeChild)
import GHCJS.DOM.EventM (on, mouseClientXY)
import GHCJS.DOM.Types (Document, HTMLElement, Window)

main = runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc
    undo <- text "aoeu" doc body
    on doc click $ do
        (x, y) <- mouseClientXY
        Just newParagraph <- createElement doc (Just "p")
        text <- createTextNode doc $ "Click " ++ show (x, y)
        appendChild newParagraph text
        appendChild body (Just newParagraph)
        return ()
    return ()


type Size = (Int, Int)
type Position = (Int, Int)

data Instance = Instance { dimensions :: Source IO (Int, Int -> Int)
                         , place :: (Size, Position) -> IO () }

type Component = Document -> HTMLElement -> IO (IO ())

type Layout = [Component] -> Component


text :: String -> Document -> HTMLElement -> IO (IO ())
text str doc parent = do
  (Just el) <- createElement doc (Just "div")
  text <- createTextNode doc str
  appendChild el text
  appendChild parent (Just el)
  return $ do
    removeChild parent (Just el)
    return ()

rootComponent :: Window -> Component -> IO (IO ())
rootComponent window c = undefined
