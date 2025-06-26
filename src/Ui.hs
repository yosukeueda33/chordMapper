{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ui (uiMain, UiInput(..), UiOutput(..)) where

import Control.Lens
import qualified Data.Text as T
import Monomer

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)

import qualified Monomer.Lens as L
import Euterpea.IO.MIDI.MidiIO

import Types

data PlayState = PlayStopped | PlayStopping | Playing deriving (Eq, Show)

data AppModel = AppModel {
  _chordName :: String,
  _playing :: PlayState,
  _progress :: Int,
  _devices :: ([(InputDeviceID, String)], [(OutputDeviceID, String)]),
  _inDev :: (InputDeviceID, String),
  _outDev :: (OutputDeviceID, String)
} deriving (Eq, Show)

data UiOutput = UiOutput {
  tChordName :: TVar String,
  tProgress :: TVar Int,
  tDevices :: TVar ([(InputDeviceID, String)], [(OutputDeviceID, String)])
}

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
  | AppUpdateProgress Int 
  | AppUpdateDevices ([(InputDeviceID, String)], [(OutputDeviceID, String)])
  deriving (Eq, Show)

data UiInput
  = UiStart InputDeviceID OutputDeviceID
  | UiStop
  | UiExit

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label_ (T.pack $ model ^. chordName) [resizeFactor 1] `styleBasic` [textSize 24],
      label_ (T.pack . genProgressString $  model ^. progress) [resizeFactor 1] `styleBasic` [textSize 24],
      selectList inDev (fst $ model ^. devices) (label . T.pack . (\(iD, name) -> show iD ++ " " ++ name)),
      selectList outDev (snd$ model ^. devices) (label . T.pack . (\(iD, name) -> show iD ++ " " ++ name)),
      button "Play/Stop" AppStartStop
    ] `styleBasic` [padding 10]

genProgressString :: Int -> String
genProgressString x
  | x >= 0 = concat . flip replicate "o" $ x `div` 6
  | otherwise = ""

handleEvent
  :: MVar UiInput -> UiOutput
  -> WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent mUiInput uiOutput wenv node model evt =
  let
    startTask = let i = fst $ model ^. inDev
                    o = fst $ model ^. outDev
                    cmd = UiStart i o 
                in putMVar mUiInput cmd >> return AppStartDone
    stopTask = putMVar mUiInput UiStop >> return AppStopDone
    disposeTask = return AppDisposeDone
    exitTask = putMVar mUiInput UiExit >> return AppExitDone
  in case evt of
    AppInit -> [ Producer $ chordUpdateProducer $ tChordName uiOutput
               , Producer $ progressProducer $ tProgress uiOutput
               , Producer $ devicesProducer $ tDevices uiOutput
               ]
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
    AppUpdateProgress x -> [Model $ model & progress .~ x]
    AppUpdateDevices xs -> [Model $ model & devices .~ xs]

chordUpdateProducer :: TVar String
                    -> (AppEvent -> IO ()) -> IO ()
chordUpdateProducer tChordName sendMsg = do
    name <- readTVarIO tChordName
    sendMsg $ AppUpdateChord name 
    threadDelay 10000
    chordUpdateProducer tChordName sendMsg

progressProducer :: TVar Int
                    -> (AppEvent -> IO ()) -> IO ()
progressProducer tProgress sendMsg = do
  readTVarIO tProgress
    >>= sendMsg . AppUpdateProgress
  threadDelay 10000
  progressProducer tProgress sendMsg

devicesProducer :: TVar ([(InputDeviceID, String)], [(OutputDeviceID, String)])
                    -> (AppEvent -> IO ()) -> IO ()
devicesProducer tDevices sendMsg = do
  readTVarIO tDevices
    >>= sendMsg . AppUpdateDevices
  threadDelay 1000000
  devicesProducer tDevices sendMsg

uiMain :: MVar UiInput -> UiOutput -> IO ()
uiMain mUiInput uiOutput = do
  startApp model (handleEvent mUiInput uiOutput) buildUI config
  where
    config = [
      appWindowTitle "KANNASHI chordMapper",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit,
      appDisposeEvent AppDispose,
      appExitEvent AppExit
      ]
    model = AppModel "" PlayStopped 0 ([], []) (unsafeInputID 0, "") (unsafeOutputID 0, "")
