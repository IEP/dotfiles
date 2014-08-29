import XMonad
import Control.Monad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.Run
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import GHC.IO.Handle.Types       as H
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Actions.CycleWS
import System.IO

--------------------------------------------------------------------------------
-- declaring vars --
--------------------------------------------------------------------------------

i       = "^i(/home/deega/icons/"
font    = "Koruri:size=10"
left    = "/home/deega/dzen/left.sh"
right   = "/home/deega/dzen/right.sh"

-- colors --

colora = "#1C1C1C"
colorb = "#D8D8D8"
colorc = "#424242"
colord = "#2E9AFE"

--------------------------------------------------------------------------------
-- okay let's start --
--------------------------------------------------------------------------------

work    = clickable $ [ i ++ "term.xbm)"
                      , i ++ "cat.xbm)"
                      , i ++ "diskette.xbm)"
                      , i ++ "docs.xbm)"
                      , i ++ "media.xbm)"
                      , i ++ "pacman.xbm)"
                      ]
       where clickable l = [ "^ca(1,xdotool key super+" ++ show(n) ++")" ++ ws ++ "^ca()" |
                 (i,ws) <- zip [1..] l, let n = i ]

mkeys = [ ((mod4Mask, xK_Return), spawn "urxvt")
        , ((mod4Mask .|. shiftMask, xK_i), spawn "firefox")
        , ((mod4Mask .|. shiftMask, xK_c), kill)
        , ((mod4Mask, xK_Left), prevWS)
        , ((mod4Mask, xK_Right), nextWS)
        , ((mod4Mask, xK_Up), sendMessage MirrorExpand)
        , ((mod4Mask, xK_Down), sendMessage MirrorShrink)
        , ((mod4Mask, xK_r), spawn "dmenu_run -b")
        , ((mod4Mask, xK_e), spawn "thunar")
        , ((mod4Mask, xK_m), spawn "urxvt -t music -e ncmpcpp")
        , ((mod4Mask .|. shiftMask, xK_r), spawn "killall dzen2 && xmonad --restart")
        , ((0, 0x1008ff13), spawn "amixer -M set Master playback 5%+ unmute")
        , ((0, 0x1008ff11), spawn "amixer -M set Master playback 5%- unmute")
        , ((0, 0x1008ff03), spawn "xbacklight -dec 10")
        , ((0, 0x1008ff02), spawn "xbacklight -inc 10")
        ]

logH h  = dynamicLogWithPP $ defaultPP
        { ppCurrent         = dzenColor (colorb) (colord) . pad
        , ppVisible         = dzenColor (colorb) (colora) . pad
        , ppHidden          = dzenColor (colorb) (colora) . pad
        , ppHiddenNoWindows = dzenColor (colorc) (colora) . pad
        , ppUrgent          = dzenColor (colord) (colora) . pad
        , ppWsSep           = " "
        , ppSep             = " "
        , ppLayout          = dzenColor (colorb) (colora) .
                     (\lay -> case lay of
                            "Spacing 2 ResizableTall" -> i ++ "strlach/dwindle.xbm)"
                            "Full"                    -> i ++ "strlach/monocle.xbm)"
                            "Spacing 2 Tall"          -> i ++ "strlach/tile.xbm)"
                            "Spacing 2 Mirror Tall"   -> i ++ "strlach/bstack.xbm)"
                            "Spacing 2 Full"          -> i ++ "strlach/monocle2.xbm)"
                     )
        , ppTitle           = (" " ++) . dzenColor (colorb) (colora) . dzenEscape . shorten 100
        , ppOrder           = \(ws:_:_:_) -> [ws]
        , ppOutput          = hPutStrLn h
        }

myH     = composeAll
        [ className =? "Firefox"            --> doShift (work !! 1)
        , resource  =? "Browser"            --> doFloat
        , resource  =? "Places"             --> doFloat
        , resource  =? "libreoffice"        --> doShift (work !! 3)
        , className =? "Vlc"                --> doShift (work !! 4)
        , className =? "Thunar"             --> doShift (work !! 2)
        , className =? "Evince"             --> doShift (work !! 3)
        , className =? "Vlc"                --> doFloat
        , isFullscreen                      --> doFullFloat
        ]

main = do
       dzen1 <- spawnPipe left
       dzen2 <- spawnPipe right
       xmonad $ defaultConfig
              { manageHook          = myH
              , layoutHook          = avoidStruts $ smartBorders $
                                      onWorkspace (work !! 1) webLayout $
                                      (spacing 2 $ layoutHook defaultConfig
                                      ||| (ResizableTall 1 (2/100) (1/2) []))
                                      ||| (noBorders (fullscreenFull Full))
              , modMask             = mod4Mask
              , focusedBorderColor  = colord
              , normalBorderColor   = colora
              , borderWidth         = 2
              , workspaces          = work
              , terminal            = "urxvt"
              , startupHook         = setWMName "LG3D"
              , logHook             = logH dzen1
              } `additionalKeys` mkeys
              where webLayout = Tall 1 (5/100) (1/2)
