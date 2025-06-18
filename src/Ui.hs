{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ui (uiMain) where

import Control.Lens
import Data.Text (Text)
import Monomer
import TextShow
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)

import qualified Monomer.Lens as L

import Types

data AppModel = AppModel {
  _clickCount :: Int,
  _chordName :: String
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppDispose
  | AppDisposeDone
  | AppExit
  | AppExitDone
  | AppUpdateChord String 
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label $ showt $ model ^. chordName,
      spacer,
      hstack [
        label $ "Click count: " <> showt (model ^. clickCount),
        spacer,
        button "Increase count" AppIncrease
      ]
    ] `styleBasic` [padding 10]

disposeTask :: TaskHandler AppEvent
disposeTask = do
    putStrLn "dispose task"
    return AppDisposeDone

exitTask :: MVar () -> TaskHandler AppEvent
exitTask exitSig = do
    putStrLn "exit task"
    putMVar exitSig ()
    return AppExitDone

handleEvent
  :: MVar ()
  -> TVar String
  -> WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent exitSig tChordName wenv node model evt = case evt of
  AppInit -> [Producer $ chordUpdateProducer tChordName]
  AppIncrease -> [Model (model & clickCount +~ 1)]
  AppDispose -> [Task disposeTask]
  AppExit -> [Task $ exitTask exitSig]
  AppUpdateChord name -> [Model $ model & chordName .~ name]

chordUpdateProducer :: TVar String
                    -> (AppEvent -> IO ()) -> IO ()
chordUpdateProducer tChordName sendMsg = do
    name <- readTVarIO tChordName
    sendMsg $ AppUpdateChord name 
    threadDelay 10000
    chordUpdateProducer tChordName sendMsg

uiMain :: MVar () -> TVar String -> IO ()
uiMain exitSig tChordName = do
  startApp model (handleEvent exitSig tChordName) buildUI config
  where
    config = [
      appWindowTitle "KANNASHI chordMapper",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit,
      appDisposeEvent AppDispose,
      appExitEvent AppExit
      ]
    model = AppModel 0 ""
