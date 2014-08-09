import XMonad
import Control.Monad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import GHC.IO.Handle.Types as H

import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

import XMonad.Actions.CycleWS (prevWS, nextWS)

import System.IO

-- begin the hell --

i = "^i(/home/deega/.xmonad/icons/"

myfn = "Koruri:Regular:size=10"
myws :: [String]
myws = clickable $ [ "^ca(3,urxvt)" ++ i ++ "term.xbm)^ca()"
                   , "^ca(3,firefox)" ++ i ++ "cat.xbm)^ca()"
                   , "^ca(3,thunar)" ++ i ++ "diskette.xbm)^ca()"
                   , i ++ "docs.xbm)"
                   , i ++ "media.xbm)"
                   , i ++ "grid.xbm)"
                   ]
       where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                           (i,ws) <- zip [1..] l, let n = i ]

mkeys = [ ((mod4Mask, xK_Return), spawn "urxvt")
        , ((mod4Mask, xK_m), spawn "urxvt -t ncmpcpp -e ncmpcpp")
        , ((mod4Mask .|. shiftMask, xK_i), spawn "firefox")
        , ((mod4Mask, xK_e), spawn "thunar")
        , ((mod4Mask .|. shiftMask, xK_c), kill)
        , ((mod4Mask, xK_Left), prevWS)
        , ((mod4Mask, xK_Right), nextWS)
        , ((mod4Mask, xK_Up), sendMessage MirrorExpand)
        , ((mod4Mask, xK_Down), sendMessage MirrorShrink)
        , ((mod4Mask, xK_r), spawn "dmenu_run -b -fn 'Inconsolata for Powerline' -nb '#EEEEEE' -nf '#0D0B0D' -sf '#FFFFFF' -sb '#007FFF'")
        , ((mod4Mask .|. shiftMask, xK_r), spawn "killall dzen2 && xmonad --restart")
        , ((0, xK_Print), spawn "xfce4-screenshooter")
        ]

black = "#0D0B0D"
grey  = "#EEEEEE"
grey1 = "#CFCFCF"
white = "#FFFFFF"
blue  = "#007FFF"
oren  = "#FF8000"
custz = "#303030"
custy = "#4F4F4F"

logBar h = do
       dynamicLogWithPP $ thePP h
thePP :: Handle -> PP
thePP h = defaultPP
        { ppCurrent         = dzenColor (white) (blue) . pad
        , ppVisible         = dzenColor (black) (grey) . pad
        , ppHidden          = dzenColor (black) (grey) . pad
        , ppHiddenNoWindows = dzenColor (grey1) (grey) . pad
        , ppUrgent          = dzenColor (blue)  (grey) . pad
        , ppWsSep           = ""
        , ppSep             = " "
        , ppLayout          = dzenColor (white) (oren) .
                  (\z -> case z of
                      "Spacing 2 ResizableTall" -> click ++ tempat ++ "dwindle.xbm)^ca() "
                      "Full"                    -> click ++ tempat ++ "monocle.xbm)^ca() "
                      "Spacing 2 Tall"          -> click ++ tempat ++ "tile.xbm)^ca() "
                      "Spacing 2 Mirror Tall"   -> click ++ tempat ++ "bstack.xbm)^ca() "
                      "Spacing 2 Full"          -> click ++ tempat ++ "monocle2.xbm)^ca() "
                  )
        , ppTitle           = ("" ++) . dzenColor (black) (grey) . shorten 100
        , ppOutput          = hPutStrLn h
        }
        where tempat = " ^i(/home/deega/.xmonad/icons/strlach/"

click :: String
click = "^ca(1, xdotool key super+space)"

res = ResizableTall 1 (2/100) (1/2) []
ful = noBorders (fullscreenFull Full)

myHook :: ManageHook
myHook = composeAll
        [ className =? "Firefox"              --> doShift (myws !! 1)
        , className =? "Google-chrome-stable" --> doShift (myws !! 1)
        , className =? "Thunar"               --> doShift (myws !! 2)
        , className =? "Evince"               --> doShift (myws !! 3)
        , className =? "libreoffice"          --> doShift (myws !! 3)
        , className =? "Vlc"                  --> doFloat
        , className =? "Vlc"                  --> doShift (myws !! 4)
        , className =? "Gimp"                 --> doFloat
        , className =? "Inkscape"             --> doFloat
        , isFullscreen                        --> doFullFloat
        ]

main = do
    panel  <- spawnPipe top
    panel1 <- spawnPipe "sh /home/deega/.xmonad/script/pk.sh"
    xmonad $ defaultConfig
        { manageHook = myHook
        , layoutHook =  avoidStruts $ smartBorders $ (spacing 2 $ layoutHook defaultConfig ||| res ) ||| ful
        , modMask = mod4Mask
        , focusedBorderColor = "#007fff"
        , normalBorderColor = "#4f4f4f"
        , borderWidth = 1
        , workspaces = myws
        , terminal = "urxvt"
        , startupHook = setWMName "LG3D"
        , logHook = logBar panel
        } `additionalKeys` mkeys
        where top = "dzen2 -p -ta l -e 'button3=' -fn '" 
                    ++ myfn ++ "' -fg '" ++ black ++ "' -bg '" ++ grey ++ "' -w 800"
                    ++ " -h 20 "
