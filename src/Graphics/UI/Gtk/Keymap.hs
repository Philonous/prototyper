{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.UI.Gtk.Keymap where

import           Data.IORef
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable

import           Control.Applicative ((<$>))
import           Control.Exception as Ex
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans

import           Graphics.UI.Gtk.Gdk.Keys (keyvalToLower)
import qualified Graphics.UI.Gtk as GTK

import           System.Glib.Flags


-- modifier flags
shift        = fromFlags [ GTK.Shift        ]
lock         = fromFlags [ GTK.Lock         ]
control      = fromFlags [ GTK.Control      ]
alt          = fromFlags [ GTK.Alt          ]
alt2         = fromFlags [ GTK.Alt2         ]
alt3         = fromFlags [ GTK.Alt3         ]
alt4         = fromFlags [ GTK.Alt4         ]
alt5         = fromFlags [ GTK.Alt5         ]
button1      = fromFlags [ GTK.Button1      ]
button2      = fromFlags [ GTK.Button2      ]
button3      = fromFlags [ GTK.Button3      ]
button4      = fromFlags [ GTK.Button4      ]
button5      = fromFlags [ GTK.Button5      ]
super        = fromFlags [ GTK.Super        ]
hyper        = fromFlags [ GTK.Hyper        ]
meta         = fromFlags [ GTK.Meta         ]
release      = fromFlags [ GTK.Release      ]
modifierMask = fromFlags [ GTK.ModifierMask ]

type KeyDef = (Int, GTK.KeyVal) -- Modifier,Key
type Keymap m = Map.Map KeyDef (m ())

data KeyState m = KeyState
  { keymapRef :: IORef [Keymap (StateT NextMap m)]
  , keymap ::Keymap (StateT NextMap m)
  , typeThroughRef :: IORef Bool
  , typeThroughOnMissmatch :: Bool
  , runAction :: m ((),NextMap) -> IO ((),NextMap)
  }

data NextMap = Reset -- reset Map to default
             | Keep  -- keep current map
             | Back  -- revert one layer

data AbortKey = AbortKey deriving (Show, Typeable)
instance Exception AbortKey

-- handleKey :: Web -> GTK.EventM GTK.EKey Bool
handleKey conf = do
  keyVal <-  keyvalToLower <$> GTK.eventKeyVal
  mods <- fromFlags <$> GTK.eventModifier
  currentKeymap <- liftIO $ head <$> readIORef (keymapRef conf)
  through <- liftIO $ readIORef $ typeThroughRef conf
  if through then do
      when (keyVal == GTK.keyFromName "Escape") .
        liftIO $ writeIORef (typeThroughRef conf) False
      return False
    else case Map.lookup (mods, keyVal) currentKeymap of
    Nothing -> return . not . typeThroughOnMissmatch $ conf
    Just action -> do
      (_, nextmap) <- liftIO $ Ex.catch (runAction conf $ runStateT action Reset)
      	                                ( \(e :: AbortKey) -> return ((),Reset) )
      case nextmap of
	 Reset -> liftIO $ writeIORef (keymapRef conf) [keymap conf]
	 Back  -> liftIO $ modifyIORef (keymapRef conf) tail
	 Keep  -> return ()
      return True


mkKeymap :: [((Int, Text), m ())] -> Keymap m
mkKeymap keyDefs = Map.fromList $ for keyDefs $ \ ((mod,keyname),action) -> ((mod, keyvalToLower $ GTK.keyFromName keyname) , action)
   where
     for = flip map

addKeymap widget typeThroughOnMissmatch runAction keymap = do
  keymapRef <- newIORef [keymap]
  typeThroughRef <- newIORef False
  GTK.on widget GTK.keyPressEvent $ handleKey KeyState{..}


-- submap :: Keymap -> WebInputMonad ()
submap map = do
  ref <- asks keymapRef
  liftIO $ modifyIORef ref (map :)
  put Keep
