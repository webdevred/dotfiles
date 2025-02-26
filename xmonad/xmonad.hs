{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- a simple configuration
import XMonad hiding ((|||))
import XMonad.Util.SpawnOnce

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WindowSwallowing
import XMonad.Layout.Fullscreen
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, spawnPipe)
import Control.Monad (filterM)

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (ByteString)
import Data.Ord (Down(..))
import Data.String (fromString)
import Data.Text (Text)
import Text.Read (readMaybe)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Lazy as TL (strip, unpack)
import qualified Data.Text.Lazy.Encoding as TLE (decodeUtf8)

import Data.Int (Int32)

import Numeric (showHex)

import GHC.Stack (withFrozenCallStack)

import qualified Data.ByteString.Lazy as BL

import Control.Exception (Exception, IOException, catch, throw, try)
import Data.Char (chr, isAscii, toLower)
import Data.List (find, genericLength, isInfixOf, isPrefixOf, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import System.IO (hPutStrLn)

import XMonad.Util.WindowProperties (getProp32s)

import Data.Hashable

import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Bimap as Bimap
import Data.Bimap (Bimap)

import Data.Ratio

import XMonad.Actions.GridSelect

-- main function
main :: IO ()
main = do
  let myBars =
        xmobarStatusBar 0 "big_screen" <>
        xmobarStatusBar 1 "small_screen_top" <>
        xmobarStatusBar 1 "small_screen_bottom"
  xmonad . ewmhFullscreen . ewmh . docks . withSB myBars $ myConfig

-- colors
pink :: String
pink = "#ffaaff"

magenta :: String
magenta = "#ff55ff"

white :: String
white = "#ffffff"

-- xmobar stuff
-- you need xmobar to be installed
xmobarStatusBar :: Int -> String -> StatusBarConfig
xmobarStatusBar screen bar = statusBarProp cmd $ pure myXmobarPP
  where
    cmd =
      "xmobar -x" ++ show screen ++ " ~/.config/xmobar/" ++ bar ++ "_config.hs"

untitledIfNull :: String -> String
untitledIfNull "" = "untitled"
untitledIfNull v = v

excludeEmojis :: String -> String
excludeEmojis = filter isEmoji
  where
    emojiRanges =
      [ (0x1F600, 0x1F64F) -- Emoticons
      , (0x1F300, 0x1F5FF) -- Miscellaneous Symbols and Pictographs
      , (0x1F680, 0x1F6FF) -- Transport and Map Symbols
      , (0x1F1E6, 0x1F1FF) -- Regional Indicator Symbols
      ]
    isEmoji :: Char -> Bool
    isEmoji c =
      not
        (any
           (\(start, end) -> c `elem` [chr x | x <- [start .. end]])
           emojiRanges)

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = xmobarPink " | "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = xmobarPink . wrap "(" ")"
    , ppVisible = xmobarPink . wrap "<" ">"
    , ppHidden = xmobarMagenta
    , ppUrgent = xmobarRed . wrap (xmobarYellow "!") (xmobarYellow "!")
    , ppLayout = map toLower
    , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap "[" "]" . xmobarPink . ppWindow
    formatUnfocused = wrap "[" "]" . xmobarMagenta . ppWindow
    ppWindow =
      map toLower . xmobarRaw . shorten 30 . excludeEmojis . untitledIfNull
    xmobarMagenta = xmobarColor magenta ""
    xmobarPink = xmobarColor pink ""
    xmobarYellow = xmobarColor "#f1fa8c" ""
    xmobarRed = xmobarColor "#ff5555" ""

-- my preffered terminal
myTerminal :: String
myTerminal = "alacritty"

myTerminalClass :: String
myTerminalClass = "Alacritty"

-- rofi
runRofi :: MonadIO m => String -> m ()
runRofi rofiType = spawn ("rofi -show " ++ rofiType)

-- general GridSelect movement
myGridNavigation :: TwoD a (Maybe a)
myGridNavigation =
  makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where
    navKeyMap =
      Map.fromList
        [ ((0, xK_Escape), cancel)
        , ((0, xK_Return), select)
        , ((0, xK_space), setPos (0, 0) >> myGridNavigation)
        , ((shiftMask, xK_7), substringSearch myGridNavigation)
        , ((0, xK_s), substringSearch myGridNavigation)
        , ((0, xK_Tab), moveNext >> navNSearch)
        , ((shiftMask, xK_Tab), movePrev >> navNSearch)
        -- arrow keys
        , ((0, xK_Left), move (-1, 0) >> myGridNavigation)
        , ((0, xK_Right), move (1, 0) >> myGridNavigation)
        , ((0, xK_Up), move (0, -1) >> myGridNavigation)
        , ((0, xK_Down), move (0, 1) >> myGridNavigation)
        -- h, j, k, l to move left, down, up and right
        , ((0, xK_h), move (-1, 0) >> myGridNavigation)
        , ((0, xK_j), move (0, 1) >> myGridNavigation)
        , ((0, xK_k), move (0, -1) >> myGridNavigation)
        , ((0, xK_l), move (1, 0) >> myGridNavigation)
        --  y and  i to move left and right and up at the same time
        , ((0, xK_u), move (-1, -1) >> myGridNavigation)
        , ((0, xK_i), move (1, -1) >> myGridNavigation)
        --  n and m to move left and right and down at the same time
        , ((0, xK_n), move (-1, 1) >> myGridNavigation)
        , ((0, xK_m), move (1, 1) >> myGridNavigation)
        ]
    -- The navigation handler ignores unknown key symbols
    navDefaultHandler = const myGridNavigation

-- my applications for Grid Select
myApplications :: [(String, String)]
myApplications =
  [ ("Brave", "brave")
  , ("wpc", "wpc")
  , ("Caja", "caja")
  , ("Chromium", "chromium")
  , ("Discord", "discord")
  , ("Emacs", "emacs-gtk")
  , ("Firefox", "firefox")
  , ("Flameshot", "flameshot launcher")
  , ("GHCI", myTerminal ++ " -T ghci -e ghci")
  , ("GIMP", "gimp")
  , ("htop", myTerminal ++ " -T htop -e htop")
  , ("Handbrake", "handbrake")
  , ("MakeMKV", "makemkv")
  , ("Midnight Commander", myTerminal ++ " -T \"Midnight Commander\" -e mc")
  , ("MongoDB Compass", "mongo-compass")
  , ("Pluma", "pluma")
  , ("Steam", "steam")
  , ("Terminal", myTerminal)
  , ("qBittorrent", "qbittorrent")
  ]

toHex :: Int -> String
toHex n =
  if length hex == 1
    then '0' : hex
    else hex
  where
    hex = showHex n ""

rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) = '#' : concatMap toHex [r, g, b]

randomInRange :: Int -> Int -> Int
randomInRange x y = abs $ hashWithSalt y y `mod` x

generateColor :: String -> (Int, Int, Int)
generateColor string =
  cond
    [ (rem hash_value 3 == 0, (255, 183 + pink_range, 193 + pink_range))
    , (rem hash_value 3 == 1, (255, 99 + red_range, 71 + red_range))
    , (True, (128 + purple_range, 0, 128 + purple_range))
    ]
  where
    pink_range = randomInRange 62 hash_value
    red_range = randomInRange 184 hash_value
    purple_range = randomInRange 127 hash_value
    hash_value = hash string

invertColor :: (Int, Int, Int) -> (Int, Int, Int)
invertColor (r, g, b) = (255 - r, 255 - g, 255 - b)

pinkColorizer :: String -> Bool -> X (String, String)
pinkColorizer this hovering =
  if hovering
    then return (pink, white)
    else return (rgbToHex bgColor, rgbToHex . invertColor $ bgColor)
  where
    bgColor = generateColor this

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst =
  gridselect
    def
      { gs_colorizer = pinkColorizer
      , gs_navigate = myGridNavigation
      , gs_bordercolor = pink
      }
    lst >>=
  flip whenJust spawn

-- my audio sink GridSelect.
-- I want to do something more complicated and funny in the future
-- but this will do for now
type SinkName = Text

type SinkDesc = String

data AudioSink =
  AudioSink
    { sink_name :: SinkName
    , sink_desc :: SinkDesc
    , sink_active :: Bool
    , sink_mute :: Bool
    }
  deriving (Show)

instance FromJSON AudioSink where
  parseJSON =
    withObject "AudioSink" $ \o -> do
      sink_name <- o .: "name"
      sink_desc <- o .: "description"
      status <- o .: "state" :: Parser Text
      let sink_active = status == "RUNNING"
      sink_mute <- o .: "mute"
      return AudioSink {..}

audioGridCellWidth :: [AudioSink] -> Integer
audioGridCellWidth = (7 *) . maximum . map (genericLength . sink_desc)

sinkToTuple :: AudioSink -> (SinkDesc, SinkName)
sinkToTuple sink = (sink_desc sink, sink_name sink)

activeSinkNotHead :: [AudioSink] -> [AudioSink]
activeSinkNotHead (f@(AudioSink {sink_active = True}):s:rest) = s : f : rest
activeSinkNotHead lst = lst

audioGridColorizer ::
     Maybe AudioSink -> [AudioSink] -> SinkName -> Bool -> X (String, String)
audioGridColorizer activeSink mutedSinks this hovering =
  cond
    [ (hovering, return (pink, "#000000"))
    , (isSinkMuted, return ("#ff0000", "#000000"))
    , (isSinkActive, return (pink, white))
    , (True, return (magenta, white))
    ]
  where
    isSinkActive = (sink_name <$> activeSink) == Just this
    isSinkMuted = any (\sink -> sink_name sink == this) mutedSinks

decodeContent :: ByteString -> Map SinkName Int
decodeContent str =
  case decode str of
    Just str' -> str'
    Nothing ->
      withFrozenCallStack $ error $ "can not decode \"" ++ faultyData ++ "\""
  where
    faultyData = TL.unpack . TL.strip . TLE.decodeUtf8 $ str

audioSinkMetrics :: String -> IO (Bimap Int SinkName)
audioSinkMetrics filename = do
  content <- tryReadFile
  case content of
    Right content' -> return . mapToBimap . decodeContent $ content'
    Left _ -> return Bimap.empty
  where
    tryReadFile :: IO (Either IOException ByteString)
    tryReadFile = try $ BL.readFile filename

mapToBimap :: Map SinkName Int -> Bimap Int SinkName
mapToBimap = Map.foldrWithKey insert' Bimap.empty
  where
    insert' sink_name metric = Bimap.insert metric sink_name

succ' :: Bimap Int SinkName -> Int -> Int
succ' m v =
  let v' = succ v
   in if Bimap.member v' m
        then succ' m v'
        else v'

-- let maxVal = toInteger . maximum . Map.elems $ m
scaleDownMap :: Bimap Int SinkName -> Bimap Int SinkName
scaleDownMap m =
  let maxVal = toInteger (maxBound :: Int)
      half x acc =
        let x' = max 1 (fromInteger $ toInteger x * maxVal `div` (2 * maxVal))
         in if Bimap.member x' acc
              then succ' acc x'
              else x'
      halfFoldingFun acc (x, k) = Bimap.insert (half x acc) k acc
   in foldl halfFoldingFun Bimap.empty $ Bimap.assocs m

incrementKey :: SinkName -> Bimap Int SinkName -> Bimap Int SinkName
incrementKey k m
  | currentValue == maxBound =
    let scaledDownMap = scaleDownMap m
     in Bimap.adjustR (succ' scaledDownMap) k scaledDownMap
  | Bimap.memberR k m = Bimap.adjustR (succ' m) k m
  | otherwise = Bimap.insert (succ' m 0) k m
  where
    currentValue = fromMaybe 0 $ Bimap.lookupR k m

sortingFun :: Bimap Int SinkName -> AudioSink -> Down (Maybe Int)
sortingFun audioSinkMetrics sink =
  Down $ Bimap.lookupR (sink_name sink) audioSinkMetrics

doAudioGridSelect :: String -> [AudioSink] -> X ()
doAudioGridSelect configLocation sinks = do
  audioSinkMetrics <- liftIO $ audioSinkMetrics filename
  let mutedSinks = filter sink_mute sinks
      activeSink = find sink_active sinks
      gridConfig =
        def
          { gs_navigate = myGridNavigation
          , gs_cellwidth = audioGridCellWidth sinks
          , gs_colorizer = audioGridColorizer activeSink mutedSinks
          , gs_bordercolor = white
          }
      prepareSinks =
        map sinkToTuple .
        activeSinkNotHead . sortOn (sortingFun audioSinkMetrics)
  sinkMaybe <- gridselect gridConfig $ prepareSinks sinks
  case sinkMaybe of
    Just sink -> do
      liftIO $
        BL.writeFile
          filename
          (encode . Bimap.toMapR $ incrementKey sink audioSinkMetrics)
      spawn $ "pactl set-default-sink " ++ T.unpack sink
    Nothing -> return ()
  where
    filename = configLocation ++ "/.audiosink.json"

audioGridSelect :: X ()
audioGridSelect = do
  configLocation <- asks (cfgDir . directories)
  output <- runProcessWithInput "pactl" ["-f", "json", "list", "sinks"] ""
  whenJust (decode . fromString $ output) $ doAudioGridSelect configLocation

data AudioWindow = AudioWindow {
  input_pid :: Int32,
  input_corked :: Bool
  } deriving (Show)
  
instance FromJSON AudioWindow where
  parseJSON =
    withObject "AudioWindow" $ \o -> do
      properties <- o .: "properties"
      pidText    <- properties .: "application.process.id"
      input_pid <- case readMaybe pidText of
             Just p -> return p
             Nothing -> fail "Failed to convert 'application.process.id' to Int32"
      input_corked <- o .: "corked"
      return AudioWindow{..}

comparePid :: Foldable t => t AudioWindow -> Window -> X Bool
comparePid aws w = do
  mPid <- getProp32s "_NET_WM_PID" w
  case mPid of
    Just [x] -> return $ any (\aw -> and [fromIntegral x == input_pid aw, not $ input_corked aw]) aws
    _        -> return False


doAudioWindowGridSelect :: [AudioWindow] -> X ()
doAudioWindowGridSelect audio_windows = do
  ws <- getAllWindows
  filtered_ws <- filterM (comparePid audio_windows) ws
  windowTitles <- getWindowTitles filtered_ws
  let gridConfig =
        def
          { gs_colorizer = fromClassName'
          , gs_navigate = myGridNavigation
          , gs_bordercolor = pink
          , gs_cellwidth = 40 * 7
          , gs_cellheight = 35
          , gs_cellpadding = 5
          }
  mWin <- gridselect gridConfig (zip windowTitles filtered_ws)
  whenJust mWin  (\w -> windows (W.focusWindow w))

audioWindowGridSelect :: X ()
audioWindowGridSelect = do
  output <- runProcessWithInput "pactl" ["-f", "json", "list", "sink-inputs"] ""
  whenJust (decode . fromString $ output) $ doAudioWindowGridSelect

-- window grid select
fromClassName' :: Window -> Bool -> X (String, String)
fromClassName' w active = runQuery className w >>= flip pinkColorizer active

getAllWindows :: X [Window]
getAllWindows = do
  windowSet <- gets windowset
  return $ W.allWindows windowSet

getWindowTitles :: [Window] -> X [String]
getWindowTitles = mapM (runQuery title)

windowGridSelect :: X ()
windowGridSelect = do
  let gridConfig =
        def
          { gs_colorizer = fromClassName'
          , gs_navigate = myGridNavigation
          , gs_bordercolor = pink
          , gs_cellwidth = 40 * 7
          , gs_cellheight = 35
          , gs_cellpadding = 5
          }
  goToSelected gridConfig

-- spawn xprop info for current window
spawnXpropInfo :: X ()
spawnXpropInfo =
  withFocused $ \w ->
    spawn $ "xprop -id 0x" ++ showHex w "" ++ " | xmessage -file -"

-- function for switching to a layout
switchToLayout :: String -> X ()
switchToLayout = sendMessage . JumpToLayout

-- my keys merged with the default keys
myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) =
  Map.union
    (Map.fromList
       [ ((modm, xK_s), spawnSelected' myApplications)
       , ((modm, xK_a), windowGridSelect)
       , ((modm, xK_p), audioGridSelect)
       , ((modm, xK_o), audioWindowGridSelect)
       , ((modm, xK_x), runRofi "drun")
       , ((modm, xK_z), runRofi "window")
       , ((modm, xK_f), switchToLayout "Full")
       , ((modm, xK_b), switchToLayout "Big Master Tall")
       , ((modm, xK_t), switchToLayout "Tall")
       , ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink)
       , ((modm .|. shiftMask, xK_i), spawnXpropInfo)
       , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
       , ( (0, xF86XK_AudioRaiseVolume)
         , spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
       , ( (0, xF86XK_AudioLowerVolume)
         , spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
       , ( (0, xF86XK_AudioMute)
         , spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
       , ((modm, xK_F1), namedScratchpadAction scratchpads "emacs-scratch")
       ]) $
  keys def conf

-- this takes the configLocation that is fetched in main
myConfig =
  def
    { borderWidth = 7
    , terminal = myTerminal
    , modMask = mod4Mask
    , normalBorderColor = magenta
    , focusedBorderColor = pink
    , focusFollowsMouse = False
    , startupHook = myStartup <+> startupHook def
    , manageHook = myManageHook <+> manageHook def
    , handleEventHook =  handleEventHook def
    , layoutHook = avoidStruts $ smartBorders myLayouts
    , keys = myKeys
    }

doShiftDynamic :: WorkspaceId -> ManageHook
doShiftDynamic ws = do
  liftX $ addHiddenWorkspace ws
  doShift ws

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "mpv" --> (doFullFloat <+> doShift "1")
    , className =? "discord" --> doShift "2"
    , className =? "steam" <&&> (not <$> title =? "Steam") -->
      (hasBorder False <+> doRectFloat (W.RationalRect 0.1 0.1 0.2 0.8))
    , className =? "firefox" <&&> resource =? "Toolkit" --> doFullFloat
    , className =? "cs2" --> doShiftDynamic "game"
    ] <+>
  namedScratchpadManageHook scratchpads

myHandleEventHook =
  swallowEventHook (className =? myTerminalClass) (return True)

scratchpads :: [NamedScratchpad]
scratchpads =
  [NS "emacs-scratch" spawnEmacsScratch findEmacsScratch manageEmacsScratch]
  where
    findEmacsScratch = title =? "emacs-scratch"
    spawnEmacsScratch =
      "emacsclient -a='' -nc -F '(quote (name . \"emacs-scratch\"))'"
    manageEmacsScratch = nonFloating

-- Tall is good for a lot of stuff
-- sometimes I really like when my Emacs or Firefox takes up 2 / 3 up of
-- the space and then I can have a few windows on the side.
-- That is why I like Big Master Tall
myLayouts =
  named "Tall" (spacing 10 $ Tall 1 (1 / 4) (1 / 2)) |||
  named "Big Master Tall" (spacing 10 $ Tall 1 0 (2 / 3)) ||| Full

-- start picom with my config and set my background using wpc
myStartup = do
  spawnOnce "picom"
  spawnOnce "wpc -b"

-- lisp like cond
cond :: [(Bool, a)] -> a
cond conditions =
  case find fst conditions of
    Just (_, result) -> result
    Nothing -> error "No true condition could be found in cond"
