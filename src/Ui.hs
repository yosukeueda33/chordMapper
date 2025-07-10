{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ui (UiInput(..), UiUpdator, createUiThread) where

import Control.Lens
import qualified Data.Text as T
import Monomer

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)

import qualified Monomer.Lens as L
import Euterpea.IO.MIDI.MidiIO

import Types

data PlayState = PlayStopped | PlayStopping | Playing deriving (Eq, Show)

type DeviceLists = ([(InputDeviceID, String)], [(OutputDeviceID, String)])

data AppModel = AppModel {
  _chordName :: String,
  _playing :: PlayState,
  _clockProgress :: Int,
  _chordSetProgress :: Int,
  _inDev :: (InputDeviceID, String),
  _outDev :: (OutputDeviceID, String)
} deriving (Eq, Show)

data UiOutput = UiOutput {
  tChordName :: TVar String,
  tClockProgress :: TVar Int,
  tChordSetProgress :: TVar Int
}

type UiUpdator = (Maybe String, Maybe Int, Maybe Int) -> IO ()

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
  | AppUpdateClockProgress Int 
  | AppUpdateChordSetProgress Int 
  deriving (Eq, Show)

data UiInput
  = UiStart InputDeviceID OutputDeviceID
  | UiStop
  | UiExit

makeLenses 'AppModel

buildUI
  :: DeviceLists
  -> WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI devices wenv model = widgetTree where
  widgetTree = vstack [
      label_ (T.pack $ model ^. chordName) [resizeFactor 1] `styleBasic` [textSize 24],
      label_ (T.pack . genProgressString 1 $  model ^. chordSetProgress) [resizeFactor 1] `styleBasic` [textSize 24],
      label_ (T.pack . genProgressString 6 $  model ^. clockProgress) [resizeFactor 1] `styleBasic` [textSize 24],
      selectList inDev (fst devices) (label . T.pack . (\(iD, name) -> show iD ++ " " ++ name)),
      selectList outDev (snd devices) (label . T.pack . (\(iD, name) -> show iD ++ " " ++ name)),
      button "Play/Stop" AppStartStop
    ] `styleBasic` [padding 10]

genProgressString :: Int -> Int -> String
genProgressString divNum x
  | x >= 0 = concat . flip replicate "o" $ x `div` divNum
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
    AppInit -> [Producer $ outputUpdateProducer uiOutput]
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
    AppExitDone -> []
    AppUpdateChord name -> [Model $ model & chordName .~ name]
    AppUpdateClockProgress x -> [Model $ model & clockProgress .~ x]
    AppUpdateChordSetProgress x -> [Model $ model & chordSetProgress .~ x]

outputUpdateProducer :: UiOutput -> (AppEvent -> IO ()) -> IO ()
outputUpdateProducer uiOutput sendMsg = do
  let
    f event elm = (event <$> (readTVarIO $ elm uiOutput))
                  >>= sendMsg
  f AppUpdateChord tChordName
  f AppUpdateClockProgress tClockProgress
  f AppUpdateChordSetProgress tChordSetProgress
  threadDelay 10000
  outputUpdateProducer uiOutput sendMsg

uiMain :: MVar UiInput -> UiOutput -> DeviceLists -> IO ()
uiMain mUiInput uiOutput devices = do
  startApp model h b config
  where
    h = handleEvent mUiInput uiOutput :: AppEventHandler AppModel AppEvent
    b = buildUI devices :: AppUIBuilder AppModel AppEvent
    config = [
      appWindowTitle "KANNASHI chordMapper",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit,
      appDisposeEvent AppDispose,
      appExitEvent AppExit
      ]
    model = AppModel "" PlayStopped 0 0 (unsafeInputID 0, "") (unsafeOutputID 0, "") :: AppModel


createUiThread :: DeviceLists -> IO (IO UiInput, UiUpdator)
createUiThread devices = do 
  mUiInput <- newEmptyMVar
  tChordName <- newTVarIO ""
  tClockProgress <- newTVarIO 0
  tChordSetProgress <- newTVarIO 0
  _ <- forkIO $ uiMain mUiInput (UiOutput tChordName tClockProgress tChordSetProgress) devices

  let
    uiInput = takeMVar mUiInput
    updateTVar tx = maybe (return ()) (atomically . writeTVar tx)
    updator (mbChordName, mbClockProgress, mbChordSetProgress)
      =  updateTVar tChordName mbChordName
      >> updateTVar tClockProgress mbClockProgress
      >> updateTVar tChordSetProgress mbChordSetProgress

  return (uiInput, updator)