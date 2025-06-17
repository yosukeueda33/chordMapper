{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ui (uiMain) where

import Control.Lens
import Data.Text (Text)
import Monomer
import TextShow
import Control.Concurrent.MVar

import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppDispose
  | AppDisposeDone
  | AppExit
  | AppExitDone
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Hello world",
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
  -> WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent exitSig wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]
  AppDispose -> [Task disposeTask]
  AppExit -> [Task $ exitTask exitSig]

uiMain :: MVar () -> IO ()
uiMain exitSig = do
  startApp model (handleEvent exitSig) buildUI config
  where
    config = [
      appWindowTitle "KANNASHI chordMapper",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit,
      appDisposeEvent AppDispose,
      appExitEvent AppExit
      ]
    model = AppModel 0
