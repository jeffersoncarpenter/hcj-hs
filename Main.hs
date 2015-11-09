module Main (
    main
) where

import Control.Applicative ((<$>))
import Control.Monad ((<=<), liftM)
import Control.Monad.Trans (lift)
import Data.Conduit
import Data.Maybe (fromJust)
import GHCJS.DOM
       (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.CSSStyleDeclaration (removeProperty, setProperty)
import GHCJS.DOM.Document (getBody, createElement, createTextNode, click, getElementById)
import GHCJS.DOM.Element (castToElement, Element, getFirstElementChild, getScrollHeight, getScrollWidth, getStyle, setAttribute, setInnerHTML)
import GHCJS.DOM.Node (appendChild, cloneNode, getChildNodes, getOwnerDocument, removeChild)
import GHCJS.DOM.NodeList (item, getLength)
import GHCJS.DOM.EventM (on, mouseClientXY)
import GHCJS.DOM.Types (Document, HTMLElement, Window)

-- we never use !important
setStyle a b c = setProperty a b c ""


main :: IO ()
main = runWebGUI $ \webView -> do
    Just doc <- webViewGetDomDocument webView
    initSandbox doc
    undo <- rootComponent doc $ textStreamComponent $ do
      yield (DisplayText "aoeu" (Font 20 "initial"))
      yield (DisplayText "aoeu aosetn uhaosen tuha osnetuh asonetu hs" (Font 20 "initial"))
    on doc click $ lift undo
    return ()

sandboxId = "sandbox"

initSandbox :: Document -> IO ()
initSandbox doc = do
  Just sandbox <- createElement doc (Just "div")
  Just body <- getBody doc
  Just style <- getStyle sandbox
  setAttribute sandbox "id" sandboxId
  setStyle style "z-index" (Just "-1")
  appendChild body (Just sandbox)
  return ()


type Dimensions = (Int, Int -> IO Int)

data Instance = Instance { dimensions :: Source IO Dimensions
                         , place :: (Int, Int) -> IO () }

type Component = HTMLElement -> IO (Instance, IO ())

type Layout = [Component] -> Component

width :: String
width = "width"

sandboxElement :: (Element -> IO a) -> Element -> IO a
sandboxElement f el = do
  Just doc <- getOwnerDocument el
  Just sandbox <- getElementById doc sandboxId
  Just el' <- (fmap . fmap) castToElement $ cloneNode el True
  appendChild sandbox (Just el')
  r <- f el'
  removeChild sandbox (Just el')
  return r

measureElement :: Element -> IO Dimensions
measureElement el = sandboxElement (\el' -> do
  Just style <- getStyle el'
  setStyle style "position" (Just "absolute")
  removeProperty style "width" :: IO (Maybe String)
  removeProperty style "height" :: IO (Maybe String)
  w <- getScrollWidth el'
  return (w, \w -> sandboxElement (\el' -> do
                                       Just style <- getStyle el'
                                       setStyle style "width" (Just $ show w ++ "px")
                                       removeProperty style "height" :: IO (Maybe String)
                                       getScrollHeight el') el)) $ el


data Font = Font { size :: Int
                 , family :: String }

data DisplayText = DisplayText String Font

empty :: Element -> IO ()
empty el = do
  Just childNodes <- getChildNodes el
  length <- getLength childNodes
  if length == 0
    then return ()
    else do
    nodes <- mapM (item childNodes) [0..(length-1)]
    mapM_ (removeChild el) nodes


applyDisplayText :: DisplayText -> Element -> IO ()
applyDisplayText (DisplayText str font) el = do
  empty el
  Just doc <- getOwnerDocument el
  text <- createTextNode doc str
  appendChild el text
  return ()


textComponent :: DisplayText -> Component
textComponent dt parent = textStreamComponent (yield dt) parent


textStreamComponent :: Source IO DisplayText -> Component
textStreamComponent dtS parent = do
  Just doc <- getOwnerDocument parent
  (Just el) <- createElement doc (Just "div")
  appendChild parent (Just el)
  let dimsS = dtS =$= awaitForever (\dt -> do
                                        lift $ applyDisplayText dt el
                                        dims <- lift $ measureElement el
                                        yield dims)
  return (Instance dimsS undefined, do
              removeChild parent (Just el)
              return ())




rootComponent :: Document -> Component -> IO (IO ())
rootComponent doc c = do
  Just body <- getBody doc
  (Instance dS place, undo) <- c body
  runConduit $ dS =$= (awaitForever $ \(w, hF) -> do
    lift (putStrLn $ show w)
    h <- lift $ hF w
    lift (putStrLn $ show h)
    return ())
  return undo
