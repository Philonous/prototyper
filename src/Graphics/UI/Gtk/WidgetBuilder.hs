{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.UI.Gtk.WidgetBuilder where

import Control.Monad.Reader
import Data.IORef
import Graphics.UI.Gtk

newtype WidgetAdder =  WA (forall w . WidgetClass w => w -> IO ())

withContainer :: (WidgetClass w) =>
                 IO w
              -> ReaderT w IO t
              -> ReaderT WidgetAdder IO (t, w)
withContainer new action = do
  WA cont <- ask
  c <- liftIO $ new
  res <- lift $ runReaderT action c
  liftIO $ cont c
  return (res,c)

addCont :: ContainerClass t =>
           ReaderT WidgetAdder IO b -> ReaderT t IO b
addCont action = do
  container <- ask
  lift $ runReaderT action (WA $ containerAdd container)

boxPackS :: BoxClass t =>
            Int
         -> Packing
         -> ReaderT WidgetAdder IO b
         -> ReaderT t IO b
boxPackS padding style  action = do
  container <- ask
  lift $ runReaderT action (WA $ \w -> boxPackStart container w style padding)

boxPackS' :: BoxClass t => Packing -> ReaderT WidgetAdder IO b -> ReaderT t IO b
boxPackS' style action = boxPackS 0 style action

packGrow :: BoxClass t => ReaderT WidgetAdder IO b -> ReaderT t IO b
packGrow = boxPackS' PackGrow

packNatural :: BoxClass t => ReaderT WidgetAdder IO b -> ReaderT t IO b
packNatural = boxPackS' PackNatural

addPage :: NotebookClass t =>
           String -> ReaderT WidgetAdder IO b -> ReaderT t IO b
addPage name action = do
  container <- ask
  lift $ runReaderT action (WA $ \w -> notebookAppendPage container w name >> return () )

withVBoxNew :: ReaderT VBox IO t -> ReaderT WidgetAdder IO (t, VBox)
withVBoxNew = withContainer (vBoxNew False 0)

withHBoxNew :: ReaderT HBox IO t -> ReaderT WidgetAdder IO (t, HBox)
withHBoxNew = withContainer (hBoxNew False 0)

withHButtonBoxNew :: ReaderT VButtonBox IO t
                  -> ReaderT WidgetAdder IO (t, VButtonBox)
withHButtonBoxNew = withContainer (vButtonBoxNew)

withScrolledWindow :: ReaderT WidgetAdder IO t
                   -> ReaderT WidgetAdder IO (t, ScrolledWindow)
withScrolledWindow = withContainer (scrolledWindowNew Nothing Nothing) . addCont

withNotebook :: ReaderT Notebook IO t -> ReaderT WidgetAdder IO (t, Notebook)
withNotebook = withContainer notebookNew

withAlignment :: Float
              -> Float
              -> Float
              -> Float
              -> ReaderT Alignment IO t
              -> ReaderT WidgetAdder IO (t, Alignment)
withAlignment xa ya xs ys = withContainer $ alignmentNew xa ya xs ys

withMainWindow :: ReaderT WidgetAdder IO t -> IO (t, Window)
withMainWindow action = do
  w <- windowNew
  a <- runReaderT action (WA $ containerAdd w)
  return (a,w)

addNewWidget :: WidgetClass b => IO b -> ReaderT WidgetAdder IO b
addNewWidget new = do
  WA cont <- ask
  w <- lift new
  lift $ cont w
  return w

addLabel :: ReaderT WidgetAdder IO Label
addLabel = addNewWidget $ labelNew Nothing

addEntry :: ReaderT WidgetAdder IO Entry
addEntry = addNewWidget entryNew

addButton :: String -> IO () -> ReaderT WidgetAdder IO Button
addButton name action = do
  b <- liftIO $ buttonNew
  liftIO $ buttonSetLabel b name
  liftIO $ on b buttonActivated action
  addNewWidget $ return b

addProgressBar :: ReaderT WidgetAdder IO ProgressBar
addProgressBar = addNewWidget $ progressBarNew

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
                -> ReaderT (TreeView, t) IO a
                -> ReaderT WidgetAdder IO TreeView
withNewTreeView model action = do
  WA cont <- ask
  treeView <- lift $ treeViewNewWithModel model
  lift $ runReaderT action (treeView, model)
  lift $ cont treeView
  return treeView

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

createLog :: ReaderT WidgetAdder IO (TextView, [Char] -> IO ())
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
