-- a simple configuration
import XMonad hiding ((|||))
import XMonad.Util.EZConfig
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
import XMonad.Util.Run (spawnPipe)

import Data.Char (toLower)
import System.IO (hPutStrLn)

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
    ppWindow = map toLower . xmobarRaw . shorten 30 . untitledIfNull
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
    } `additionalKeysP`
  [ ("M-s", spawnSelected' myApplications)
  , ("M-x", runRofi "drun" configLocation)
  , ("M-z", runRofi "window" configLocation)
  , ("M-S-f", sendMessage $ JumpToLayout "Full")
  , ("M-S-b", sendMessage $ JumpToLayout "Big Master Tall")
  , ("M-S-t", sendMessage $ JumpToLayout "Tall")
  , ("M-C-<Space>", namedScratchpadAction scratchpads "emacs-scratch")
  ]

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect def lst >>= flip whenJust spawn

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
