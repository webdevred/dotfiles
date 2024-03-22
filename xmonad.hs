-- a simple configuration

import XMonad
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

import System.IO (hPutStrLn)

import XMonad.Layout.LayoutModifier
import XMonad.Hooks.DynamicLog

main :: IO()
main = do
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar ~/.xmonad/xmobar/config.hs" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = pink " | "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap "(" ")"
    , ppVisible         = pink . wrap "<" ">"
    , ppHidden          = magenta
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap "[" "]" . pink    . ppWindow
    formatUnfocused = wrap "[" "]" . magenta . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    pink, magenta, red, yellow :: String -> String
    magenta  = xmobarColor "#ff55ff" ""
    pink     = xmobarColor "#ffaaff" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""


-- myXmobarPP = def xmobarPP {ppOrder  = \(ws:l:t:_) -> [ws] }

myConfig = def
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
         ("M-C-<Space>", namedScratchpadAction scratchpads "emacs-scratch"),
         ("M-v", spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
        ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "mpv" --> (doFullFloat <+> doShift "1"),
      className =? "discord" --> doShift "2"]
    <+> namedScratchpadManageHook scratchpads

scratchpads :: [NamedScratchpad]
scratchpads = [NS "emacs-scratch" spawnEmacsScratch findEmacsScratch manageEmacsScratch]
  where findEmacsScratch = title =? "emacs-scratch"
        spawnEmacsScratch = "emacsclient -a='' -nc -F '(quote (name . \"emacs-scratch\"))'"
        manageEmacsScratch = nonFloating

myLayouts = Tall 1 (1/2) (1/2) ||| Tall 1 (1/2) (2/3) ||| Full
  

myStartup = do
  spawnOnce "feh --bg-fill /mnt/HDD/bakgrund2.jpg /mnt/HDD/bakgrund.jpg"


-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
