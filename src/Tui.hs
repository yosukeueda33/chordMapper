{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui where

import Data.Bool (bool)
import Data.Maybe (fromJust)
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.List as L
import qualified Data.Vector as Vec
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget, ViewportType(Vertical))
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
import qualified Brick.Widgets.List as WL
import Euterpea.IO.MIDI.MidiIO
    ( OutputDeviceID, InputDeviceID, unsafeInputID, unsafeOutputID )
import Brick.BChan (BChan, newBChan, writeBChan)

data UiInput
  = UiStart InputDeviceID OutputDeviceID
      (Maybe OutputDeviceID) -- For like, lights on keyboard like MiniLab3 touch pads.
  | UiStop
  | UiExit

data CustomEvent = ChangeChordInfo String Int Int

type UiUpdator = (Maybe String, Maybe Int, Maybe Int) -> IO ()
type DeviceLists = ([(InputDeviceID, String)], [(OutputDeviceID, String)])

data Name = InputList | OutputList | SpecialList | StartStopButton
          deriving (Eq, Ord, Show)

data St = St { _chordName :: String 
             , _clockProgress :: Int
             , _chordSetProgress :: Int
             , _selectListIndex :: Int
             , _inputList :: WL.List Name String
             , _outputList :: WL.List Name String
             , _specialList :: WL.List Name String
             , _playing :: Bool
             }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st = L.singleton
          $ hBox [
                      colHigh 0 (str "Select Input:") <=>
                        WL.renderList renderFunc True (st^.inputList)
                   ,  colHigh 1 (str "Select Output:") <=>
                        WL.renderList renderFunc True (st^.outputList)
                   ,  colHigh 2 (str "Select Special output:") <=>
                        WL.renderList renderFunc True (st^.specialList)
                   ] <=>
            strWrap ( "Chord             : " ++ st^.chordName ) <=>
            strWrap ( "Chord set progress: "
              ++ concat (replicate (st^.chordSetProgress) "o")) <=>
            strWrap ( "Chord progress    : "
              ++ concat (replicate (st^.clockProgress `div` 6) "o")) <=>
            (withAttr (attrName "info")
              . strWrap $ "Left/Right: Select device type."
                          ++ "  Up/Down: Select device."
                          ++ "  Space: Start/Stop playing.")
  where
    colHigh i = if i == st^.selectListIndex then withAttr (attrName "colHighlight") else id
    renderFunc sel e = rowHigh $ strWrap e
      where rowHigh = if sel then withAttr (attrName "rowHighlight") else id
      
appEvent :: (Int -> Int -> Int -> IO ())
         -> IO () -> T.BrickEvent Name CustomEvent -> T.EventM Name St ()
appEvent _ _ (T.AppEvent (ChangeChordInfo name clockI setI))
                                          =  chordName %= const name
                                          >> clockProgress %= const clockI
                                          >> chordSetProgress %= const setI
appEvent start stop (T.VtyEvent e) = case e of
  (V.EvKey V.KLeft []) -> selectListIndex %= (\i -> max 0 (i - 1))
  (V.EvKey V.KRight []) -> selectListIndex %= (\i -> min 2 (i + 1))
  (V.EvKey (V.KChar ' ') []) ->
    use playing >>= bool
                      (do
                        inId <- use $ inputList . WL.listSelectedL
                        outId <- use $ outputList . WL.listSelectedL
                        spId <- use $ specialList . WL.listSelectedL
                        liftIO $ start (fromJust inId)
                                       (fromJust outId)
                                       (fromJust spId)
                        playing %= const True)
                      (liftIO stop >> playing %= const False)
  (V.EvKey V.KEsc []) -> do
                           vty <- M.getVtyHandle
                           liftIO $ V.setMode (V.outputIface vty) V.Mouse False
                           liftIO $ putStr "\ESC[0m\ESC[2J\ESC[H\ESC[?25h"
                           liftIO $ V.shutdown vty
                           M.halt
  ev -> do
                      nowCol <- use selectListIndex
                      let targetList = case nowCol of
                                         0 -> inputList
                                         1 -> outputList
                                         2 -> specialList
                                         _ -> inputList
                      T.zoom targetList $ WL.handleListEvent ev
appEvent _ _ _ =  return ()

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ (attrName "colHighlight",   V.white `on` V.cyan)
    , (attrName "rowHighlight",   V.white `on` V.blue)
    , (attrName "info",           V.white `on` V.magenta)
    ]

app :: (Int -> Int -> Int -> IO ()) -> IO () -> M.App St CustomEvent Name
app start stop =
    M.App { M.appDraw = drawUi
          , M.appChooseCursor = M.showFirstCursor
          , M.appStartEvent = do
              vty <- M.getVtyHandle
              liftIO $ V.setMode (V.outputIface vty) V.Mouse True
          , M.appHandleEvent = appEvent start stop
          , M.appAttrMap = const aMap
          }

tuiMain :: DeviceLists
        -> (Int -> Int -> Int -> IO ()) -> IO () -> IO ()
        -> BChan CustomEvent
        -> IO()
tuiMain devices start stop exit varsChan = do
  let
    inputs = Vec.fromList . map (\(id, name) -> show id ++ ": " ++ name) $ fst devices 
    outputs = Vec.fromList . map (\(id, name) -> show id ++ ": "++ name) $ snd devices 
    specials = outputs
  _ <- M.customMainWithDefaultVty (Just varsChan) (app start stop)
        $ St { _chordName = ""
             , _clockProgress = 0
             , _chordSetProgress = 0
             , _selectListIndex = 0
             , _inputList = WL.list InputList inputs 5
             , _outputList = WL.list OutputList outputs 5
             , _specialList = WL.list SpecialList specials 5
             , _playing = False
             }
  exit

readVars :: BChan CustomEvent -> TVar String -> TVar Int -> TVar Int -> IO ()
readVars varsChan tChordName tClockProgress tChordSetProgress = go 
  where
    go = do
      threadDelay 100000
      writeBChan varsChan =<< ChangeChordInfo <$> readTVarIO tChordName
                                              <*> readTVarIO tClockProgress
                                              <*> readTVarIO tChordSetProgress
      go
  

createTuiThread :: DeviceLists -> Bool -> IO (IO UiInput, UiUpdator)
createTuiThread devices needOutSubDev = do 
  mUiInput <- newEmptyMVar
  tChordName <- newTVarIO ""
  tClockProgress <- newTVarIO 0
  tChordSetProgress <- newTVarIO 0
  varsChan <- newBChan 10
  void . forkIO $ readVars varsChan tChordName tClockProgress tChordSetProgress
  _ <- forkIO $ tuiMain devices
                  (\inIx outIx spIx -> 
                    let
                      inId = fst $ fst devices !! inIx
                      outId = fst $ snd devices !! outIx
                      mSpId = Just . fst $ snd devices !! spIx
                    in putMVar mUiInput $ UiStart inId outId mSpId)
                  (putMVar mUiInput UiStop)
                  (putMVar mUiInput UiExit)
                  varsChan
  let
    uiInput = takeMVar mUiInput
    updateTVar tx = maybe (return ()) (atomically . writeTVar tx)
    updator (mbChordName, mbClockProgress, mbChordSetProgress)
      =  updateTVar tChordName mbChordName
      >> updateTVar tClockProgress mbClockProgress
      >> updateTVar tChordSetProgress mbChordSetProgress

  return (uiInput, updator)
