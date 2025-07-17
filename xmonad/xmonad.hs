{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- a simple configuration

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bool (bool)
import Data.ByteString.Lazy (ByteString)
import Data.Char (ord, toLower)
import Data.Hashable
import Data.Int (Int32)
import Data.List (find, isPrefixOf, sortOn)
import Data.Map (Map)
import Data.Ord (Down (..))
import Data.String (fromString)
import Data.Text (Text)
import GHC.Stack (withFrozenCallStack)
import Graphics.X11.ExtraTypes.XF86
import Numeric (showHex)
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import Text.Read (readMaybe)
import XMonad hiding ((|||))
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties (getProp32s)

import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Text qualified as T (unpack)
import Data.Text.Lazy qualified as TL (strip, unpack)
import Data.Text.Lazy.Encoding qualified as TLE (decodeUtf8)
import XMonad.StackSet qualified as W

type ConfigBar = (String, String)

decodeBars :: ByteString -> Map String [String]
decodeBars str =
  case decode str of
    Just str' -> str'
    Nothing ->
      withFrozenCallStack $ error ("can not decode \"" ++ faultyData ++ "\"")
  where
    faultyData = TL.unpack . TL.strip . TLE.decodeUtf8 $ str

listConfigBars :: IO [ConfigBar]
listConfigBars = do
  path <- getXdgDirectory XdgConfig "xmonad"
  content <- tryReadFile path
  case content of
    Right content' ->
      pure . flattenConfigBars . Map.assocs . decodeBars $ content'
    Left _ -> pure []
  where
    flattenConfigBars xs = [(a, b) | (a, bs) <- xs, b <- bs]
    tryReadFile :: FilePath -> IO (Either IOException ByteString)
    tryReadFile path = try . BL.readFile $ path ++ "/bars.json"

-- main function
main :: IO ()
main = do
  configBars <- listConfigBars
  let myBars = foldMap xmobarStatusBar configBars
   in xmonad . ewmhFullscreen . ewmh . docks . withSB myBars $ myConfig

-- colors
pink :: String
pink = "#ffaaff"

magenta :: String
magenta = "#ff55ff"

white :: String
white = "#ffffff"

-- xmobar stuff
-- you need xmobar to be installed
xmobarStatusBar :: ConfigBar -> StatusBarConfig
xmobarStatusBar (screen, bar) = statusBarProp cmd $ pure myXmobarPP
  where
    cmd = "xmobar -x" ++ screen ++ " ~/.config/xmobar/" ++ bar

untitledIfNull :: String -> String
untitledIfNull v = bool v "untitled" $ null v

emojiRanges :: [(Int, Int)]
emojiRanges =
  [ (0x1F600, 0x1F64F) -- Emoticons
  , (0x1F300, 0x1F5FF) -- Miscellaneous Symbols and Pictographs
  , (0x1F680, 0x1F6FF) -- Transport and Map Symbols
  , (0x1F1E6, 0x1F1FF) -- Regional Indicator Symbols (flags)
  , (0x2600, 0x26FF) -- Miscellaneous Symbols (includes some weather, astrology)
  , (0x2700, 0x27BF) -- Dingbats (including some emoji-like symbols)
  , (0x1F900, 0x1F9FF) -- Supplemental Symbols and Pictographs (newer emojis: food, animals, etc.)
  , (0x1FA70, 0x1FAFF) -- Symbols and Pictographs Extended-A (newer emojis like crafts, hands)
  , (0x1F700, 0x1F77F) -- Alchemical Symbols (less common emojis, but sometimes used)
  , (0x1F780, 0x1F7FF) -- Geometric Shapes Extended
  , (0x1F800, 0x1F8FF) -- Supplemental Arrows-C
  , (0x1F1E6, 0x1F1FF) -- Regional Indicator Symbols (flags, repeated for clarity)
  , (0x1F000, 0x1F02F) -- Mahjong Tiles and Domino Tiles (some used as emojis)
  , (0x1F0A0, 0x1F0FF) -- Playing Cards
  , (0x1F018, 0x1F270) -- Various Symbols and Pictographs
  , (0x1F650, 0x1F67F) -- Ornamental Dingbats
  , (0x1F1E6, 0x1F1FF) -- Flags (repetition for completeness)
  , (0x1F3FB, 0x1F3FF) -- Emoji Modifier Fitzpatrick skin tone modifiers
  , (0x200D, 0x200D) -- Zero Width Joiner (used to combine emojis)
  , (0x30FB, 0x30BFB) -- Katakana middle dot '・'
  , (8967, 8967) -- '⌇'
  ]

excludeEmojis :: String -> String
excludeEmojis = filter isNotEmoji
  where
    isNotEmoji code = not $ any (inEmojiRange $ ord code) emojiRanges
    inEmojiRange code (start, end) = code >= start && code <= end

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
        , -- arrow keys
          ((0, xK_Left), move (-1, 0) >> myGridNavigation)
        , ((0, xK_Right), move (1, 0) >> myGridNavigation)
        , ((0, xK_Up), move (0, -1) >> myGridNavigation)
        , ((0, xK_Down), move (0, 1) >> myGridNavigation)
        , -- h, j, k, l to move left, down, up and right
          ((0, xK_h), move (-1, 0) >> myGridNavigation)
        , ((0, xK_j), move (0, 1) >> myGridNavigation)
        , ((0, xK_k), move (0, -1) >> myGridNavigation)
        , ((0, xK_l), move (1, 0) >> myGridNavigation)
        , --  y and  i to move left and right and up at the same time
          ((0, xK_u), move (-1, -1) >> myGridNavigation)
        , ((0, xK_i), move (1, -1) >> myGridNavigation)
        , --  n and m to move left and right and down at the same time
          ((0, xK_n), move (-1, 1) >> myGridNavigation)
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
generateColor string
  | rem hash_value 3 == 0 = (255, 183 + pink_range, 193 + pink_range)
  | rem hash_value 3 == 1 = (255, 99 + red_range, 71 + red_range)
  | otherwise = (128 + purple_range, 0, 128 + purple_range)
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
    then pure (pink, white)
    else pure (rgbToHex bgColor, rgbToHex . invertColor $ bgColor)
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
    lst
    >>= flip whenJust spawn

-- my audio sink GridSelect.
-- I want to do something more complicated and funny in the future
-- but this will do for now
type SinkName = Text

type SinkDesc = String

data AudioSink = AudioSink
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
      pure AudioSink {..}

audioGridCellWidth :: [AudioSink] -> Integer
audioGridCellWidth =
  let sinkDescLength = toInteger . length . sink_desc
   in (7 *) . maximum . map sinkDescLength

sinkToTuple :: AudioSink -> (SinkDesc, SinkName)
sinkToTuple sink = (sink_desc sink, sink_name sink)

activeSinkNotHead :: [AudioSink] -> [AudioSink]
activeSinkNotHead (f@(AudioSink {sink_active = True}) : s : rest) = s : f : rest
activeSinkNotHead lst = lst

audioGridColorizer
  :: Maybe AudioSink -> [AudioSink] -> SinkName -> Bool -> X (String, String)
audioGridColorizer activeSink mutedSinks this hovering
  | hovering = pure (pink, "#000000")
  | isSinkMuted = pure ("#ff0000", "#000000")
  | isSinkActive = pure (pink, white)
  | otherwise = pure (magenta, white)
  where
    isSinkActive = (sink_name <$> activeSink) == Just this
    isSinkMuted = any (\sink -> sink_name sink == this) mutedSinks

decodeContent :: ByteString -> Map SinkName Int
decodeContent str =
  case decode str of
    Just str' -> str'
    Nothing ->
      withFrozenCallStack $ error ("can not decode \"" ++ faultyData ++ "\"")
  where
    faultyData = TL.unpack . TL.strip . TLE.decodeUtf8 $ str

getAudioSinkMetrics :: String -> IO (Map SinkName Int)
getAudioSinkMetrics filename = do
  content <- tryReadFile
  case content of
    Right content' -> pure . decodeContent $ content'
    Left _ -> pure Map.empty
  where
    tryReadFile :: IO (Either IOException ByteString)
    tryReadFile = try $ BL.readFile filename

scaleDownMap :: Map SinkName Int -> Map SinkName Int
scaleDownMap m =
  let half x = max 2 $ div x 2
      halfFoldingFun k x = Map.insert k (half x)
   in Map.foldrWithKey halfFoldingFun Map.empty m

incrementKey :: SinkName -> Map SinkName Int -> Map SinkName Int
incrementKey k m
  | currentValue == maxBound =
      let scaledDownMap = scaleDownMap m
       in Map.adjust (2 +) k scaledDownMap
  | Map.member k m = Map.adjust succ k m
  | otherwise = Map.insert k 2 m
  where
    currentValue = sum $ Map.lookup k m

sortingFun :: Map SinkName Int -> AudioSink -> Down (Maybe Int)
sortingFun audioSinkMetrics' sink =
  Down $ Map.lookup (sink_name sink) audioSinkMetrics'

doAudioGridSelect :: String -> [AudioSink] -> X ()
doAudioGridSelect configLocation sinks = do
  audioSinkMetrics <- liftIO $ getAudioSinkMetrics filename
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
        map sinkToTuple
          . activeSinkNotHead
          . sortOn (sortingFun audioSinkMetrics)
  sinkMaybe <- gridselect gridConfig $ prepareSinks sinks
  case sinkMaybe of
    Just sink -> do
      liftIO $
        BL.writeFile filename (encode $ incrementKey sink audioSinkMetrics)
      spawn $ "pactl set-default-sink " ++ T.unpack sink
    Nothing -> pure ()
  where
    filename = configLocation ++ "/audiosink.json"

audioGridSelect :: X ()
audioGridSelect = do
  configLocation <- asks (cfgDir . directories)
  output <- runProcessWithInput "pactl" ["-f", "json", "list", "sinks"] ""
  whenJust (decode . fromString $ output) $ doAudioGridSelect configLocation

data AudioWindow = AudioWindow
  { input_pid :: Int32
  , input_corked :: Bool
  }
  deriving (Show)

instance FromJSON AudioWindow where
  parseJSON =
    withObject "AudioWindow" $ \o -> do
      properties <- o .: "properties"
      pidText <- properties .: "application.process.id"
      input_pid <-
        case readMaybe pidText of
          Just p -> pure p
          Nothing -> fail "Failed to convert 'application.process.id' to Int32"
      input_corked <- o .: "corked"
      pure AudioWindow {..}

comparePid :: [AudioWindow] -> Window -> X Bool
comparePid aws w = do
  mPid <- getProp32s "_NET_WM_PID" w
  case mPid of
    Just [x] ->
      let winInAudioWindowList aw = fromIntegral x == input_pid aw && not (input_corked aw)
       in pure $ any winInAudioWindowList aws
    _ -> pure False

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
  whenJust mWin (windows . W.focusWindow)

audioWindowGridSelect :: X ()
audioWindowGridSelect = do
  output <- runProcessWithInput "pactl" ["-f", "json", "list", "sink-inputs"] ""
  whenJust (decode . fromString $ output) doAudioWindowGridSelect

-- window grid select
fromClassName' :: Window -> Bool -> X (String, String)
fromClassName' w active = runQuery className w >>= flip pinkColorizer active

getAllWindows :: X [Window]
getAllWindows = do
  windowSet <- gets windowset
  pure $ W.allWindows windowSet

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
    ( Map.fromList
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
        ,
          ( (0, xF86XK_AudioRaiseVolume)
          , spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
          )
        ,
          ( (0, xF86XK_AudioLowerVolume)
          , spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
          )
        ,
          ( (0, xF86XK_AudioMute)
          , spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
          )
        , ((modm, xK_F1), namedScratchpadAction scratchpads "emacs-scratch")
        ]
    )
    $ keys def conf

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
    , handleEventHook = handleEventHook def
    , layoutHook = avoidStruts $ smartBorders myLayouts
    , keys = myKeys
    }

doShiftDynamic :: WorkspaceId -> ManageHook
doShiftDynamic ws = liftX (addHiddenWorkspace ws) >> doShift ws

(=^?) :: Query String -> String -> Query Bool
q =^? prefix = fmap (isPrefixOf prefix) q

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "mpv" --> (doFullFloat <+> doShift "1")
    , className =? "discord" --> doShift "2"
    , className
        =? "steam"
        <&&> (not <$> title =? "Steam")
        --> (hasBorder False <+> doRectFloat (W.RationalRect 0.1 0.1 0.2 0.8))
    , className =? "firefox" <&&> resource =? "Toolkit" --> doFullFloat
    , className =? "cs2" --> doShiftDynamic "game"
    , className =^? "steam_app_" --> doShiftDynamic "game"
    ]
    <+> namedScratchpadManageHook scratchpads

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
  named "Tall" (spacing 20 $ Tall 1 (1 / 4) (1 / 2))
    ||| named "Big Master Tall" (spacing 20 $ Tall 1 0 (2 / 3))
    ||| Full

-- start picom with my config and set my background using wpc
myStartup :: X ()
myStartup = do
  spawnOnce "xset s off; xset s noblank; xset -dpms"
  spawnOnce "picom"
  spawnOnce "wpc -b"
