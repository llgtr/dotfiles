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

main :: IO()
main =
  xmonad
    $ withNavigation2DConfig def { layoutNavigation = [("BSP", hybridNavigation)] }
    $ myConfig

startUp :: X()
startUp =
    spawnOnce "nitrogen --restore"

myManageHook = composeAll
    [ className =? "Thunar" --> doFloat
    , className =? "Ristretto" --> doFloat
    ]

myConfig = def
  { borderWidth        = 0
  , focusFollowsMouse  = True
  , handleEventHook    = docksEventHook
  , keys               = myKeys
  , layoutHook         = avoidStruts $ spacingWithEdge 4 emptyBSP
  , manageHook         = manageDocks <+> myManageHook
  , startupHook        = startUp
  , modMask            = mod4Mask
  , clickJustFocuses   = False
  , terminal           = "urxvt"
  , workspaces         = [ "1", "2", "3", "4", "5" ]
  }

