{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ui (uiMain, UiInput(..)) where

import Control.Lens
import Data.Text (Text)
import Monomer
import TextShow
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)

import qualified Monomer.Lens as L

import Types

data PlayState = PlayStopped | PlayStopping | Playing deriving (Eq, Show)

data AppModel = AppModel {
  _chordName :: String,
  _playing :: PlayState
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppStartStop
  | AppStartDone
  | AppStopDone
  | AppSetBorder [Int]
  | AppDispose
  | AppDisposeDone
  | AppExit
  | AppExitDone
  | AppUpdateChord String 
  deriving (Eq, Show)

data UiInput
  = UiStart
  | UiStop
  | UiExit

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label $ showt $ model ^. chordName,
      button "Play/Stop" AppStartStop
    ] `styleBasic` [padding 10]


handleEvent
  :: MVar UiInput
  -> TVar String
  -> WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent mUiInput tChordName wenv node model evt =
  let
    startTask = putMVar mUiInput UiStart >> return AppStartDone
    stopTask = putMVar mUiInput UiStop >> return AppStopDone
    disposeTask = return AppDisposeDone
    exitTask = putMVar mUiInput UiExit >> return AppExitDone
  in case evt of
    AppInit -> [Producer $ chordUpdateProducer tChordName]
    AppStartStop -> case model ^. playing of
                      Playing -> [ Task stopTask
                                 , Model $ model & playing .~ PlayStopping
                                 , Model $ model & chordName .~ ""
                                 ]
                      PlayStopped -> [Task startTask, Model $ model & playing .~ Playing]
                      PlayStopping -> []
    AppStartDone -> []
    AppStopDone -> [Model $ model & playing .~ PlayStopped]
    AppDispose -> [Task disposeTask]
    AppExit -> [Task exitTask]
    AppUpdateChord name -> [Model $ model & chordName .~ name]

chordUpdateProducer :: TVar String
                    -> (AppEvent -> IO ()) -> IO ()
chordUpdateProducer tChordName sendMsg = do
    name <- readTVarIO tChordName
    sendMsg $ AppUpdateChord name 
    threadDelay 10000
    chordUpdateProducer tChordName sendMsg

uiMain :: MVar UiInput -> TVar String -> IO ()
uiMain mUiInput tChordName = do
  startApp model (handleEvent mUiInput tChordName) buildUI config
  where
    config = [
      appWindowTitle "KANNASHI chordMapper",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit,
      appDisposeEvent AppDispose,
      appExitEvent AppExit
      ]
    model = AppModel "" PlayStopped
