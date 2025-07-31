module Tui where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import Brick (Widget, simpleMain, (<+>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Euterpea.IO.MIDI.MidiIO
    ( OutputDeviceID, InputDeviceID, unsafeInputID, unsafeOutputID )

data UiInput
  = UiStart InputDeviceID OutputDeviceID
      (Maybe OutputDeviceID) -- For like, lights on keyboard like MiniLab3 touch pads.
  | UiStop
  | UiExit

type UiUpdator = (Maybe String, Maybe Int, Maybe Int) -> IO ()
type DeviceLists = ([(InputDeviceID, String)], [(OutputDeviceID, String)])


ui :: Widget ()
ui =
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Hello!")
      (center (str "Left") <+> vBorder <+> center (str "Right"))

tuiMain :: MVar UiInput -> IO ()
tuiMain mUiInput = do
  _ <- simpleMain ui
  putMVar mUiInput UiExit  

createTuiThread :: DeviceLists -> Bool -> IO (IO UiInput, UiUpdator)
createTuiThread devices needOutSubDev = do 
  mUiInput <- newEmptyMVar
  tChordName' <- newTVarIO ""
  tClockProgress' <- newTVarIO 0
  tChordSetProgress' <- newTVarIO 0
  _ <- forkIO $ tuiMain mUiInput
  let
    uiInput = takeMVar mUiInput
    updateTVar tx = maybe (return ()) (atomically . writeTVar tx)
    updator (mbChordName, mbClockProgress, mbChordSetProgress)
      =  updateTVar tChordName' mbChordName
      >> updateTVar tClockProgress' mbClockProgress
      >> updateTVar tChordSetProgress' mbChordSetProgress

  return (uiInput, updator)
