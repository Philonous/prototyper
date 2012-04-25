{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, PackageImports, TypeFamilies, DeriveDataTypeable #-}
module Graphics.UI.Prototyper where

import           Prelude                   hiding (log, catch)

import           Control.Applicative       ((<$>))
import           Control.Concurrent        (yield)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader

import           Data.IORef
import           Data.List                 (sortBy)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Ord                  (comparing)
import qualified Data.Traversable          as T
import           Data.Tree

import           Graphics.UI.Gtk           as GTK

import           Graphics.UI.Gtk.Keymap
import           Graphics.UI.Gtk.WidgetBuilder


data ReaderState = RS
  { statusbar     :: Label
  , logStr        :: String -> IO ()
  , logWindow     :: ScrolledWindow
  , mainView      :: Widget
  , inputBox      :: HBox
  , inputLabel    :: Label
  , inputEntry    :: Entry
  , inputAction   :: IORef (String -> ReaderMonad ())
  }


type ReaderMonad a = ReaderT ReaderState IO a

---------------------------
-- Input ------------------
---------------------------

activateInput prefill = do
  bar <- asks inputEntry
  container <- asks inputBox
  liftIO $ do
    GTK.widgetShow container
  -- grabbing focus seems to select the whole content of the entry
  -- so we do it first and remove the selection afterwards
    GTK.widgetGrabFocus bar
    set bar [ GTK.entryText := prefill
            , GTK.editablePosition := (-1)
            ]

deactivateInput = do
  widget <- asks inputBox
  view <- asks mainView
  actionRef <- asks inputAction
  liftIO $ do
    GTK.widgetHide widget
    GTK.widgetGrabFocus view
    writeIORef actionRef (const $ return ())

withInput labelText prefill action =  do
  label <- asks inputLabel
  actionRef <- asks inputAction
  liftIO $ writeIORef actionRef action
  liftIO $ GTK.labelSetText label labelText
  activateInput prefill

withInput' t p a = do
  r <- ask
  withInput t p (runReaderT a r)

-- showInfoLabel text = do
--   label <- asks infoLabel
--   liftIO $ do
--     GTK.labelSetText label text
--     GTK.widgetShow label

-- hideInfoLabel = do
--   asks infoLabel >>= liftIO . GTK.widgetHide

-- withInfoLabelInput info label prefill action = do
--   showInfoLabel info
--   withInput label prefill $ \x ->  action x >> hideInfoLabel

------------------------
-- UI Main -------------
------------------------

uiMain mainview myKeymap= do
  initGUI
  timeoutAddFull (yield >> return True) priorityDefaultIdle 100 -- keep threads alive
  actionRef <- newIORef (const $ return ())
  (((view, bar, logWindow, logToWindow, label, entry, inputBox),_),window) <- withMainWindow $ do
    withVBoxNew $ do
      v <- packGrow mainview
      ((logWindow, logStr),logScroll) <- packGrow $ withScrolledWindow createLog
      liftIO $ scrolledWindowSetPolicy logScroll PolicyNever PolicyAutomatic
      bar <- packNatural $ addLabel
      ((label, entry),inputBox) <- packNatural . withHBoxNew $ do
        label <- packNatural addLabel
        entry <- packGrow    addEntry
        return (label, entry)
      return (v, bar, logScroll, logStr, label, entry, inputBox)
  let globalState = RS
       { mainView     =  view
       , statusbar    =  bar
       , logStr       =  \l -> logToWindow l >> GTK.labelSetText bar l
       , logWindow    =  logWindow
       , inputBox     =  inputBox
       , inputLabel   =  label
       , inputEntry   =  entry
       , inputAction  =  actionRef
       }
  on entry entryActivate $ do
    text <- entryGetText entry
    action <- readIORef actionRef
    flip runReaderT globalState $ do
      deactivateInput
      action text
  addKeymap view True (`runReaderT`  globalState )  myKeymap
  windowSetDefaultSize window 800 600
  widgetShowAll window
  widgetHide logWindow
  widgetHide inputBox
  onDestroy window mainQuit
  mainGUI
