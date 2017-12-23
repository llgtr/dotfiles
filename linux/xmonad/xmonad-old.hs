import XMonad

import XMonad.Actions.Navigation2D
import XMonad.Config
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.FixedColumn
import XMonad.Layout.Spacing
import XMonad.Util.CustomKeys
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import Data.List

import MyKeyBindings

main :: IO ()
main = do
  xmobarPipe <- spawnPipe xmobarCommand
  xmonad
    $ withNavigation2DConfig def { layoutNavigation = [("BSP", hybridNavigation)] }
    $ myConfig { logHook = dynamicLogWithPP $ myXmobarPP xmobarPipe }

backgroundColor   = "#f2f0ec"
middleColor       = "#a09f93"
foregroundColor   = "#393939"

startUp :: X()
startUp = do
    spawnOnce "nitrogen --restore"

myManageHook = composeAll
    [ className =? "Thunar" --> doFloat
    ]

myConfig = def
  { borderWidth        = 1
  , focusedBorderColor = foregroundColor
  , focusFollowsMouse  = False
  , handleEventHook    = docksEventHook
  , keys               = myKeys
  , layoutHook         = avoidStruts $ spacingWithEdge 4 emptyBSP
  , manageHook         = manageDocks <+> myManageHook
  , startupHook        = startUp
  , modMask            = mod4Mask
  , normalBorderColor  = middleColor
  , clickJustFocuses   = False
  , terminal           = "urxvt"
  , workspaces         = [ "1", "2", "3", "4", "5" ]
  }

myXmobarPP xmobarPipe = defaultPP
  { ppCurrent         = pad . xmobarColor foregroundColor  ""
  , ppHidden          = pad . xmobarColor middleColor ""
  , ppHiddenNoWindows = pad . xmobarColor middleColor ""
  , ppLayout          = const ""
  , ppOutput          = hPutStrLn xmobarPipe
  , ppTitle           = const ""
  , ppVisible         = pad . xmobarColor middleColor ""
  , ppWsSep           = " "
  }

xmobarCommand :: String
xmobarCommand =
  intercalate " "
    [ "xmobar"
    , "~/.xmonad/xmobarrc"
    , "-d"
    , "-B", stringed backgroundColor
    , "-F", stringed foregroundColor
    ]
      where stringed x = "\"" ++ x ++ "\""
