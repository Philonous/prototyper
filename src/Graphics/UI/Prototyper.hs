{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.Prototyper where

import           Control.Lens hiding (set)
import           Control.Monad.State
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude hiding (log, catch)

import           Control.Applicative ((<$>))
import           Control.Concurrent (yield)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader

import           Data.IORef
import           Data.List (sortBy)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Ord (comparing)
import qualified Data.Traversable as T
import           Data.Tree

import           Graphics.UI.Gtk as GTK

import           Graphics.UI.Gtk.Keymap as Keymap
import           Graphics.UI.Gtk.WidgetBuilder




type ReaderMonad a = ReaderT ReaderState IO a

---------------------------
-- Input ------------------
---------------------------

chars :: [Char]
chars = ['1' .. '9'] ++ ['0'] ++ ['a'..'z']

chooserKeymap :: (TreeViewClass self, MonadIO m, Ord t, Num t) =>
                 self
              -> Map.Map (t, KeyVal) (m ())
chooserKeymap view = mkKeymap . flip map (zip chars [0..])
                  $ \(c, i) -> ( (0, Text.singleton c)
                               , (liftIO $ treeViewSetCursor view [i] Nothing))

chooser :: MkGUI c (TreeView, ListStore (Char, (String, IO ())))
chooser = do
    store <- liftIO $ listStoreNew []
    treeView <- withNewTreeView store $ do
        keyC <- addColumn "key" [ textRenderer (return . (: "") . fst) ]
        entryC <- addColumn "entry" [ textRenderer (return . fst . snd)]
        lift $ set entryC [treeViewColumnExpand := True]
    liftIO $ on treeView rowActivated $ \path _ -> do
        case path of
            [i] -> do
                (_, (_, action)) <- listStoreGetValue store i
                action
            _   -> error $ "chooser: expected plain path, got " ++ show path
    liftIO $ addKeymap treeView True id $ chooserKeymap treeView
    return (treeView, store)


withChoice :: (MonadReader ReaderState m, MonadIO m) =>
              [(String, ReaderT ReaderState IO a)]
           -> (a -> ReaderT ReaderState IO ())
           -> m ()
withChoice choices action = do
    (treeView, store) <- view chooseWindow
    r <- ask
    liftIO $ do
        listStoreClear store
        forM (zip chars choices) $ \(acc, (sStr, a)) ->
            listStoreAppend store (acc, (sStr, flip runReaderT r $
                                            deactivateChoice >> a >>= action))
        widgetShow treeView
        GTK.widgetGrabFocus treeView

withChoice' :: [(String, a)]
               -> (a -> ReaderT ReaderState IO ())
               -> ReaderT ReaderState IO ()
withChoice' ch a = withChoice (map (\(str, x) -> (str, return x)) ch) a

deactivateChoice :: ReaderT ReaderState IO ()
deactivateChoice = do
    (treeView, store) <- view chooseWindow
    mv <- view mainView
    liftIO $ do
        widgetHide treeView
        listStoreClear store
        widgetGrabFocus mv

activateInput :: String
              -> ReaderT ReaderState IO ()
activateInput prefill = do
  bar <- view inputEntry
  container <- view inputBox
  liftIO $ do
    GTK.widgetShow container
  -- grabbing focus seems to select the whole content of the entry
  -- so we do it first and remove the selection afterwards
    GTK.widgetGrabFocus bar
    set bar [ GTK.entryText := prefill
            , GTK.editablePosition := (-1)
            ]

deactivateInput :: ReaderT ReaderState IO ()
deactivateInput = do
  widget <- view inputBox
  mView <- view mainView
  actionRef <- view inputAction
  liftIO $ do
    GTK.widgetHide widget
    GTK.widgetGrabFocus mView
    writeIORef actionRef (const $ return ())

withInput :: String
          -> String
          -> (String -> ReaderMonad ())
          -> ReaderT ReaderState IO ()
withInput labelText prefill action =  do
  label <- view inputLabel
  actionRef <- view inputAction
  liftIO $ writeIORef actionRef action
  liftIO $ GTK.labelSetText label labelText
  activateInput prefill

setStatus :: (MonadReader ReaderState m, MonadIO m) => String -> m ()
setStatus txt = do
    bar <- view statusbar
    liftIO $ labelSetText bar txt

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

uiMain :: MkGUI VBox Widget
       -> Keymap.Keymap (StateT NextMap (ReaderT ReaderState IO))
       -> IO a
       -> IO ()
uiMain (MkGUI mainview) myKeymap action = do
  initGUI
  timeoutAddFull (yield >> return True) priorityDefaultIdle 100 -- keep threads alive
  actionRef <- newIORef (const $ return ())
  (((bar, logWindow, logToWindow, label, entry, inputBox, ch@(treeView, store), viewContainer), _ ), window) <- withMainWindow undefined $ do
      withVBoxNew $ do
          vc <- addNewWidget $ vBoxNew False 0
          ch <- packGrow chooser
          ((logWindow, logStr),logScroll) <- packGrow
                                                $ withScrolledWindow createLog
          liftIO $ scrolledWindowSetPolicy logScroll PolicyNever PolicyAutomatic
          bar <- packNatural $ addLabel Nothing
          ((label, entry),inputBox) <- packNatural . withHBoxNew $ do
              label <- packNatural $ addLabel Nothing
              entry <- packGrow    addEntry
              return (label, entry)
          return (bar, logScroll, logStr, label, entry, inputBox, ch, vc)
  let globalState = RS
             { _mainView     = toWidget viewContainer
             , _statusbar    = bar
             , _logStr       = \l -> logToWindow l >> GTK.labelSetText bar l
             , _logWindow    = logWindow
             , _inputBox     = inputBox
             , _inputLabel   = label
             , _inputEntry   = entry
             , _chooseWindow = ch
             , _inputAction  = actionRef
             }
  liftIO $ runReaderT mainview (GUIState globalState
                                         (\w -> boxPackStart viewContainer w
                                                             PackGrow 0)
                                         viewContainer)
  on entry entryActivate $ do
    text <- entryGetText entry
    action <- readIORef actionRef
    flip runReaderT globalState $ do
        deactivateInput
        action text
  addKeymap viewContainer True (`runReaderT`  globalState )  myKeymap
  windowSetDefaultSize window 800 600
  widgetShowAll window
  widgetHide logWindow
  widgetHide inputBox
  widgetHide treeView
  onDestroy window mainQuit
  action
  mainGUI
