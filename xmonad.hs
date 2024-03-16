-- a simple configuration

import XMonad
import XMonad.Util.EZConfig

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP


import XMonad.Hooks.DynamicLog

main :: IO()
main = xmonad =<< xmobar myConfig

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig = def
       { borderWidth        = 7
       , terminal = "xterm"
       , modMask = mod4Mask
       , normalBorderColor  = "#ffaaff"
       , focusedBorderColor = "#ffeeff"
       , focusFollowsMouse = False}
     `additionalKeysP`
     [ ("M-f", spawn "firefox"  ),
       ("M-a", spawn "emacs-gtk"),
       ("M-d", spawn "discord"),
       ("M-s", spawn "flameshot launcher")]
