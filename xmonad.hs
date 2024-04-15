-- a simple configuration
import XMonad hiding ((|||))
import XMonad.Util.SpawnOnce

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, spawnPipe)

import Data.Char (chr, isAscii, toLower)
import Data.List (isInfixOf, sortOn)
import System.IO (hPutStrLn)

import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as Map
import Data.Map (Map)

import System.Directory (doesFileExist, getHomeDirectory)

import XMonad.Actions.GridSelect

main :: IO ()
main = do
  configLocation <- determineConfigLocation
  let myBars =
        xmobarStatusBar configLocation 0 "big_screen" <>
        xmobarStatusBar configLocation 1 "small_screen_top" <>
        xmobarStatusBar configLocation 1 "small_screen_bottom"
  xmonad . ewmhFullscreen . ewmh . withEasySB myBars defToggleStrutsKey $
    myConfig configLocation

determineConfigLocation :: IO FilePath
determineConfigLocation = do
  homeDirectory <- getHomeDirectory
  inXmonad <- doesFileExist (homeDirectory ++ "/xmonad/xmonad.hs")
  inDotXmonad <- doesFileExist (homeDirectory ++ "/.xmonad/xmonad.hs")
  inDotConfigXmonad <-
    doesFileExist (homeDirectory ++ "/.config/xmonad/xmonad.hs")
  if inXmonad
    then return $ homeDirectory ++ "/xmonad"
    else if inDotXmonad
           then return $ homeDirectory ++ "/.xmonad"
           else if inDotConfigXmonad
                  then return $ homeDirectory ++ "/.config/xmonad"
                  else error "can not find config location"

xmobarStatusBar :: String -> Int -> String -> StatusBarConfig
xmobarStatusBar configLocation screen bar = statusBarProp cmd $ pure myXmobarPP
  where
    cmd =
      "xmobar -x" ++
      show screen ++ " " ++ configLocation ++ "/xmobar/" ++ bar ++ "_config.hs"

untitledIfNull :: String -> String
untitledIfNull "" = "untitled"
untitledIfNull v = v

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = pink " | "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = pink . wrap "(" ")"
    , ppVisible = pink . wrap "<" ">"
    , ppHidden = magenta
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppLayout = map toLower
    , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap "[" "]" . pink . ppWindow
    formatUnfocused = wrap "[" "]" . magenta . ppWindow
    ppWindow =
      map toLower . xmobarRaw . shorten 30 . excludeEmojis . untitledIfNull
    pink, magenta, red, yellow :: String -> String
    magenta = xmobarColor "#ff55ff" ""
    pink = xmobarColor "#ffaaff" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""

myTerminal :: String
myTerminal = "alacritty"

myApplications :: [(String, String)]
myApplications =
  [ ("Caja", "caja")
  , ("Discord", "discord")
  , ("Emacs", "emacs-gtk")
  , ("Firefox", "firefox")
  , ("Flameshot", "flameshot launcher")
  , ("GIMP", "gimp")
  , ("Steam", "steam")
  , ("Terminal", myTerminal)
  ]

runRofi :: MonadIO m => String -> String -> m ()
runRofi rofiType configLocation =
  spawn
    ("rofi -show " ++
     rofiType ++ " -config " ++ configLocation ++ "/rofi/config.rasi")

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
        (any (\(start, end) -> elem c [chr x | x <- [start .. end]]) emojiRanges)

switchToLayout :: String -> X ()
switchToLayout = sendMessage . JumpToLayout

pairs :: [String] -> [(String, String)]
pairs (namn:besk:rest) = (besk, namn) : pairs rest
pairs _ = []

audioGridCellWidth :: [(String, String)] -> Integer
audioGridCellWidth =
  (7 *) . toInteger . maximum . map (\(beskrivning, _) -> length beskrivning)

audioGridSelect :: X ()
audioGridSelect = do
  output <- runProcessWithInput "pactl" ["list", "sinks"] ""
  let sinks =
        sortOn fst .
        pairs .
        map
          (dropWhile (== ' ') .
           drop 1 . dropWhile (/= ':') . dropWhile (`elem` " \t")) .
        filter containsNamnOrBeskrivning . lines $
        output
      gridConfig =
        def
          { gs_navigate = myGridNavigation
          , gs_cellwidth = audioGridCellWidth sinks
          }
  sinkMaybe <- gridselect gridConfig sinks
  case sinkMaybe of
    Just sink -> spawn $ "pactl set-default-sink " ++ sink
    Nothing -> return ()
  where
    containsNamnOrBeskrivning line =
      isInfixOf "Namn" line || isInfixOf "Beskrivning" line

myKeys :: String -> XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys configLocation conf@(XConfig {modMask = modm}) =
  Map.union
    (Map.fromList
       [ ((modm, xK_s), spawnSelected' myApplications)
       , ((modm, xK_p), audioGridSelect)
       , ((modm, xK_x), runRofi "drun" configLocation)
       , ((modm, xK_z), runRofi "window" configLocation)
       , ((modm .|. shiftMask, xK_f), switchToLayout "Full")
       , ((modm .|. shiftMask, xK_b), switchToLayout "Big Master Tall")
       , ((modm .|. shiftMask, xK_t), switchToLayout "Tall")
       , ( (0, xF86XK_AudioRaiseVolume)
         , spawn "amixer -D pulse sset Master 10%+")
       , ( (0, xF86XK_AudioLowerVolume)
         , spawn "amixer -D pulse sset Master 10%-")
       , ((0, xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle")
       , ( (modm .|. shiftMask, xK_space)
         , namedScratchpadAction scratchpads "emacs-scratch")
       ]) $
  keys def conf

myConfig configLocation =
  def
    { borderWidth = 7
    , terminal = myTerminal
    , modMask = mod4Mask
    , normalBorderColor = "#ff55ff"
    , focusedBorderColor = "#ffaaff"
    , focusFollowsMouse = False
    , startupHook = myStartup configLocation <+> startupHook def
    , manageHook = myManageHook <+> manageHook def
    , layoutHook = avoidStruts $ smartBorders myLayouts
    , keys = myKeys configLocation
    }

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

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst =
  gridselect def {gs_navigate = myGridNavigation} lst >>= flip whenJust spawn

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "mpv" --> (doFullFloat <+> doShift "1")
    , className =? "discord" --> doShift "2"
    , className =? "steam" <&&> (not <$> title =? "Steam") -->
      doRectFloat (W.RationalRect 0.1 0.1 0.2 0.8)
    ] <+>
  namedScratchpadManageHook scratchpads

scratchpads :: [NamedScratchpad]
scratchpads =
  [NS "emacs-scratch" spawnEmacsScratch findEmacsScratch manageEmacsScratch]
  where
    findEmacsScratch = title =? "emacs-scratch"
    spawnEmacsScratch =
      "emacsclient -a='' -nc -F '(quote (name . \"emacs-scratch\"))'"
    manageEmacsScratch = nonFloating

myLayouts =
  named "Tall" (spacing 10 $ Tall 1 (1 / 2) (1 / 2)) |||
  named "Big Master Tall" (spacing 10 $ Tall 1 (1 / 2) (2 / 3)) ||| Full

myStartup configLocation = do
  spawnOnce ("picom --config=" ++ configLocation ++ "/picom/picom.conf")
  spawnOnce "feh --bg-fill /mnt/HDD/bakgrund2.jpg /mnt/HDD/bakgrund.jpg"
