-- a simple configuration

import XMonad hiding ((|||))
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks 
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Layout.Fullscreen  
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle 
import XMonad.Layout.MultiToggle.Instances
import qualified XMonad.StackSet as W
import XMonad.Util.Loggers
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Hidden
import XMonad.Layout.Named
import XMonad.Layout.Spacing

import System.IO (hPutStrLn)
import Data.Char (toLower)

import System.Directory (getHomeDirectory,doesFileExist)

import XMonad.Layout.LayoutModifier
import XMonad.Hooks.DynamicLog

main :: IO()
main = do
  configLocation <- determineConfigLocation
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp ("xmobar "++ configLocation ++"/xmobar/config.hs") (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig configLocation

determineConfigLocation :: IO FilePath
determineConfigLocation = do
  homeDirectory <- getHomeDirectory
  inXmonad <- doesFileExist (homeDirectory ++ "/xmonad/xmonad.hs")
  inDotXmonad <- doesFileExist (homeDirectory ++ "/.xmonad/xmonad.hs")
  inDotConfigXmonad <- doesFileExist (homeDirectory ++ "/.config/xmonad/xmonad.hs")
  if inXmonad then
    return $ homeDirectory ++ "/xmonad"
  else
    if inDotXmonad then
      return $ homeDirectory ++ "/.xmonad"
    else
      if inDotConfigXmonad then
        return $ homeDirectory ++ "/.config/xmonad"
      else
        error "can not find config location"

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = pink " | "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = pink . wrap "(" ")"
    , ppVisible         = pink . wrap "<" ">"
    , ppHidden          = magenta
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppLayout          = map toLower
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap "[" "]" . pink    . ppWindow
    formatUnfocused = wrap "[" "]" . magenta . ppWindow

    ppWindow :: String -> String
    ppWindow = map toLower . xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    pink, magenta, red, yellow :: String -> String
    magenta  = xmobarColor "#ff55ff" ""
    pink     = xmobarColor "#ffaaff" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""


-- myXmobarPP = def xmobarPP {ppOrder  = \(ws:l:t:_) -> [ws] }

myConfig configLocation = 
  def
       { borderWidth        = 7
       , terminal = "alacritty"
       , modMask = mod4Mask
       , normalBorderColor  = "#ff55ff"
       , focusedBorderColor = "#ffaaff"
       , focusFollowsMouse = False
       , startupHook = myStartup <+> startupHook def
       , manageHook = myManageHook <+> manageHook def
       , layoutHook = avoidStruts $ smartBorders $ myLayouts
    }
     `additionalKeysP`
       [ ("M-a", spawn "emacs-gtk"),
         ("M-s", spawn "flameshot launcher"),
         ("M-c", spawn "caja"),
         ("M-x", spawn "rofi -show drun -config ~/.xmonad/rofi/config.rasi"),
         ("M-z", spawn "rofi -show window -config ~/.xmonad/rofi/config.rasi"),
         ("M-S-f", sendMessage $ JumpToLayout "Full"),
         ("M-S-b", sendMessage $ JumpToLayout "Big Master Tall"),
         ("M-S-t", sendMessage $ JumpToLayout "Tall"),
         ("M-C-<Space>", namedScratchpadAction scratchpads "emacs-scratch")
        ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "mpv" --> (doFullFloat <+> doShift "1"),
      className =? "discord" --> doShift "2",
      (className =? "steam" <&&> (not <$> title =? "Steam") --> doRectFloat (W.RationalRect 0.1 0.1 0.2 0.8))]
    <+> namedScratchpadManageHook scratchpads

scratchpads :: [NamedScratchpad]
scratchpads = [NS "emacs-scratch" spawnEmacsScratch findEmacsScratch manageEmacsScratch]
  where findEmacsScratch = title =? "emacs-scratch"
        spawnEmacsScratch = "emacsclient -a='' -nc -F '(quote (name . \"emacs-scratch\"))'"
        manageEmacsScratch = nonFloating

myLayouts =
  named "Tall" (spacing 10 $ Tall 1 (1/2) (1/2))
  ||| named "Big Master Tall" (spacing 10 $ Tall 1 (1/2) (2/3))
  ||| Full
  

myStartup = do
  spawnOnce "feh --bg-fill /mnt/HDD/bakgrund2.jpg /mnt/HDD/bakgrund.jpg"
