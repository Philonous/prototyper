{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.UI.Gtk.WidgetBuilder where

import           Control.Applicative
import           Control.Lens hiding (set)
import           Control.Lens.TH
import           Control.Monad.Reader
import qualified Data.Foldable as DF
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.UI.Gtk

data ReaderState = RS
  { _statusbar     :: Label
  , _logStr        :: String -> IO ()
  , _logWindow     :: ScrolledWindow
  , _mainView      :: Widget
  , _inputBox      :: HBox
  , _inputLabel    :: Label
  , _inputEntry    :: Entry
  , _chooseWindow  :: (TreeView, ListStore (Char, (String, IO ())))
  , _inputAction   :: IORef (String -> ReaderT ReaderState IO ())
  }

makeLenses ''ReaderState

toggleLog :: (MonadReader ReaderState m, MonadIO m) => m ()
toggleLog = do
    w <- view logWindow
    liftIO $ set w [widgetVisible :~ not]

logToWindow :: (MonadReader ReaderState m, MonadIO m) => String -> m ()
logToWindow str = do
    ls <- view logStr
    liftIO $ ls str

data GUIState c = GUIState { _readerState :: ReaderState
                           , _widgetAdder :: Widget -> IO ()
                           , _container :: c
                           }

makeLenses ''GUIState

newtype MkGUI c a = MkGUI {fromMkGUI :: ReaderT (GUIState c) IO a}
                  deriving (Functor,Applicative, Monad, MonadIO)

getRState = MkGUI $ view readerState

contAdd :: WidgetClass w => w -> MkGUI c ()
contAdd w = do
    add <- MkGUI $ view widgetAdder
    liftIO $ add (toWidget w)

withContainer :: (ContainerClass t,ContainerClass c) =>
                 IO c
              -> MkGUI c a
              -> MkGUI t (a, c)
withContainer new action = do
  c <- liftIO new
  st <- getRState
  let st' = GUIState st (containerAdd c) c
  contAdd c
  res <- liftIO $ runReaderT (fromMkGUI action) st'
  return (res,c)

withBoxContainer :: (BoxClass c, ContainerClass t) =>
                    IO c
                 -> MkGUI c a
                 -> MkGUI t (a, c)
withBoxContainer new action = withContainer new (packNatural action)

setWA :: (c -> Widget -> IO ()) -> MkGUI c a -> MkGUI c a
setWA f (MkGUI action) = MkGUI $ do
  st <- ask
  liftIO $ runReaderT action (st & widgetAdder .~ f (st ^. container))

boxPackS :: BoxClass t =>
            Int
         -> Packing
         -> MkGUI t a
         -> MkGUI t a
boxPackS padding style = setWA $ \c w -> boxPackStart c w style padding

boxPackS' :: BoxClass t => Packing -> MkGUI t a -> MkGUI t a
boxPackS' style action = boxPackS 0 style action

packGrow :: BoxClass t => MkGUI t a -> MkGUI t a
packGrow = boxPackS' PackGrow

packRepel :: BoxClass t => MkGUI t a -> MkGUI t a
packRepel = boxPackS' PackRepel

grow :: BoxClass t => MkGUI t a -> MkGUI t a
grow = packGrow

packNatural :: BoxClass t => MkGUI t a -> MkGUI t a
packNatural = boxPackS' PackNatural

addPage :: NotebookClass n =>
           Text
        -> MkGUI n a
        -> MkGUI n a
addPage name = setWA (\c w -> notebookAppendPage c w name >> return ())

withVBoxNew :: ContainerClass c => MkGUI VBox a -> MkGUI c (a, VBox)
withVBoxNew = withBoxContainer (vBoxNew False 0)

withHBoxNew :: ContainerClass t => MkGUI HBox a -> MkGUI t (a, HBox)
withHBoxNew = withBoxContainer (hBoxNew False 0)

withFrame :: (ContainerClass c) =>
             Maybe Text
          -> MkGUI Frame a
          -> MkGUI c (a, Frame)
withFrame mbLabel = withContainer $ do
    fr <- frameNew
    DF.forM_ mbLabel $ \label -> liftIO $ frameSetLabel fr label
    return fr

withHButtonBoxNew :: ContainerClass c =>
                     MkGUI VButtonBox a
                  -> MkGUI c (a, VButtonBox)
withHButtonBoxNew = withBoxContainer (vButtonBoxNew)

withScrolledWindow :: ContainerClass c =>
                      MkGUI ScrolledWindow a
                   -> MkGUI c (a, ScrolledWindow)
withScrolledWindow = withContainer $ scrolledWindowNew Nothing Nothing

withNotebook :: ContainerClass c => MkGUI Notebook a -> MkGUI c (a, Notebook)
withNotebook = withContainer notebookNew

withAlignment :: ContainerClass t =>
                 Float
              -> Float
              -> Float
              -> Float
              -> MkGUI Alignment a
              -> MkGUI t (a, Alignment)
withAlignment xa ya xs ys = withContainer $ alignmentNew xa ya xs ys

withMainWindow :: ReaderState -> MkGUI Window t -> IO (t, Window)
withMainWindow st (MkGUI action) = do
  w <- windowNew
  a <- runReaderT action (GUIState st (containerAdd w) w)
  return (a,w)

addNewWidget :: WidgetClass w => IO w -> MkGUI c w
addNewWidget new = do
  w <- liftIO new
  contAdd w
  return w


addLabel :: Maybe Text -> MkGUI c Label
addLabel mbLabelText = addNewWidget $ labelNew mbLabelText

addEntry :: MkGUI c Entry
addEntry = addNewWidget entryNew

addButton :: Text -> ReaderT ReaderState IO () -> MkGUI c Button
addButton name action = do
  b <- liftIO $ buttonNew
  liftIO $ buttonSetLabel b name
  st <- MkGUI $ view readerState
  liftIO $ on b buttonActivated $ runReaderT action st
  addNewWidget $ return b

addProgressBar :: MkGUI c ProgressBar
addProgressBar = addNewWidget progressBarNew

addColumn :: (CellRendererClass cell, TreeViewClass self) =>
             String
          -> [ReaderT (self, t) IO  (cell, TreeViewColumn -> t -> IO b)]
          -> ReaderT (self, t) IO TreeViewColumn
addColumn name renderers= do
  (view, model) <- ask
  col <- lift $ treeViewColumnNew
  lift $ set col [treeViewColumnTitle := name]
  forM_ renderers $ \i -> do
    (rend, attr) <- i
    lift $ do
      treeViewColumnPackStart col rend True
      attr col model
  lift $ treeViewAppendColumn view col
  return col

withNewTreeView :: TreeModelClass t =>
                   t
                -> ReaderT (TreeView, t) IO ()
                -> MkGUI c TreeView
withNewTreeView model action = do
  treeView <- liftIO $ treeViewNewWithModel model
  liftIO $ runReaderT action (treeView, model)
  contAdd treeView
  return treeView


withTreeViewListStore :: ReaderT (TreeView, ListStore a) IO ()
                      -> (a -> ReaderT ReaderState IO ())
                      -> MkGUI c (ListStore a, TreeView)
withTreeViewListStore action onItem = do
    store <- liftIO $ listStoreNew []
    st <- MkGUI $ view readerState
    treeView <- withNewTreeView store action
    liftIO $ on treeView rowActivated $ \path _ -> case path of
        [i] -> execReaderT st . onItem =<< listStoreGetValue store i
        _   -> error $ "chooser: expected plain path, got " ++ show path
    return (store, treeView)
  where
    execReaderT = flip runReaderT

setListStore :: ListStore a -> [a] -> IO ()
setListStore store list = do
    listStoreClear store
    forM_ list $ listStoreAppend store

-- | cellLayoutSetAttributes
cLSA :: (TypedTreeModelClass model, CellLayoutClass self,
         CellRendererClass cell, TreeModelClass (model row)) =>
        cell
     -> (row -> [AttrOp cell])
     -> self
     -> model row
     -> IO ()
cLSA rend attr col model = cellLayoutSetAttributes col rend model attr

textRenderer :: (TypedTreeModelClass model, CellLayoutClass self,
                 TreeModelClass (model row), MonadIO m) =>
                (row -> IO String)
             -> m (CellRenderer, self -> model row -> IO ())
textRenderer selector = liftIO $ do
  rend <- cellRendererTextNew
  set rend [cellTextEllipsize := EllipsizeEnd]
  return (toCellRenderer rend, cLSA rend $ \row -> [cellText :=> selector row])

textBufferAppendLine :: TextBufferClass self => self -> [Char] -> IO ()
textBufferAppendLine buffer entry = do
  end <- textBufferGetEndIter buffer
  textBufferInsert buffer end (entry ++ "\n")

createLog :: MkGUI c (TextView, [Char] -> IO ())
createLog = do
  text <- liftIO $ textViewNew
  buffer <- liftIO $ textViewGetBuffer text
  addNewWidget (return text)
  return (text, textBufferAppendLine buffer)

withTreeModelFilter :: (TypedTreeModelClass model, TreeModelClass (model t)) =>
                       IORef (t -> IO Bool)
                    -> IO (model t)
                    -> IO (model t, TypedTreeModelFilter t)
withTreeModelFilter ref createModel = do
  model <- createModel
  treeModelFilter <- treeModelFilterNew model []
  let filter iter = {-# SCC "filter" #-}do
--        iter <- treeModelFilterConvertIterToChildIter treeModelFilter iter'
--        print iter
        path <- treeModelGetPath model iter
        case path of
          [] -> error "empty path"
                             -- not catching this case or returning True will
                             -- crash the program with seg fault.
                             -- This problem only seems to appear
                             -- when adding the first element to an initially
                             -- empty child store
          _  -> do
--                 print path
                 row <- customStoreGetRow model iter
                 filter <- readIORef ref
                 res <- filter row
--                 putStrLn "done filtering this"
                 return res
  treeModelFilterSetVisibleFunc treeModelFilter filter
  return (model, treeModelFilter)
