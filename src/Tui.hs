{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui where

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

data UiInput
  = UiStart InputDeviceID OutputDeviceID
      (Maybe OutputDeviceID) -- For like, lights on keyboard like MiniLab3 touch pads.
  | UiStop
  | UiExit

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
             }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st = L.singleton
          $ C.vCenterLayer $
              C.hCenterLayer $
                hBox [
                        colHigh 0 (str "Select Input:") <=>
                          WL.renderList renderFunc True (st^.inputList)
                     ,  colHigh 1 (str "Select Output:") <=>
                          WL.renderList renderFunc True (st^.outputList)
                     ,  colHigh 2 (str "Select Special output:") <=>
                          WL.renderList renderFunc True (st^.specialList)
                     ]
  where
    colHigh i = if i == st^.selectListIndex then withAttr (attrName "colHighlight") else id
    renderFunc sel e = rowHigh $ strWrap e
      where rowHigh = if sel then withAttr (attrName "rowHighlight") else id
      
appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent ev@(T.MouseDown n _ _ loc) = return ()
appEvent (T.MouseUp {}) = return ()
appEvent (T.VtyEvent (V.EvMouseUp {})) = return ()
appEvent (T.VtyEvent (V.EvKey V.KLeft [])) = selectListIndex %= (\i -> max 0 (i - 1))
appEvent (T.VtyEvent (V.EvKey V.KRight [])) = selectListIndex %= (\i -> min 2 (i + 1))
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent (T.VtyEvent ev) = do
  nowCol <- use selectListIndex
  let targetList = case nowCol of
                     0 -> inputList
                     1 -> outputList
                     2 -> specialList
                     _ -> inputList
  T.zoom targetList $ WL.handleListEvent ev
appEvent _ = return ()

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ (attrName "colHighlight",   V.white `on` V.cyan)
    , (attrName "rowHighlight",   V.white `on` V.magenta)
    ]

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appChooseCursor = M.showFirstCursor
          , M.appStartEvent = do
              vty <- M.getVtyHandle
              liftIO $ V.setMode (V.outputIface vty) V.Mouse True
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          }

tuiMain :: DeviceLists -> MVar UiInput -> IO ()
tuiMain devices mUiInput = do
  let
    inputs = Vec.fromList . map (\(id, name) -> show id ++ ": " ++ name) $ fst devices 
    outputs = Vec.fromList . map (\(id, name) -> show id ++ ": "++ name) $ snd devices 
    specials = outputs
  _ <- M.defaultMain app $ St "" 0 0 0
                             (WL.list InputList inputs 5)
                             (WL.list OutputList outputs 5)
                             (WL.list SpecialList specials 5)
  putMVar mUiInput UiExit  

createTuiThread :: DeviceLists -> Bool -> IO (IO UiInput, UiUpdator)
createTuiThread devices needOutSubDev = do 
  mUiInput <- newEmptyMVar
  tChordName' <- newTVarIO ""
  tClockProgress' <- newTVarIO 0
  tChordSetProgress' <- newTVarIO 0
  _ <- forkIO $ tuiMain devices mUiInput
  let
    uiInput = takeMVar mUiInput
    updateTVar tx = maybe (return ()) (atomically . writeTVar tx)
    updator (mbChordName, mbClockProgress, mbChordSetProgress)
      =  updateTVar tChordName' mbChordName
      >> updateTVar tClockProgress' mbClockProgress
      >> updateTVar tChordSetProgress' mbChordSetProgress

  return (uiInput, updator)
