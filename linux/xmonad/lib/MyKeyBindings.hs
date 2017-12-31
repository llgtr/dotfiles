module MyKeyBindings where

import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D

import XMonad.Layout
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.ToggleLayouts

import XMonad.Util.CustomKeys

import Graphics.X11.ExtraTypes.XF86

myKeys = customKeys removedKeys addedKeys

removedKeys :: XConfig l -> [(KeyMask, KeySym)]
removedKeys XConfig {modMask = modm} =
  [ (modm              , xK_space)  -- Default for layout switching
  , (modm .|. shiftMask, xK_Return) -- Default for opening a terminal
  , (modm .|. shiftMask, xK_c)      -- Default for closing the focused window
  ]

addedKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
addedKeys conf @ XConfig {modMask = modm} =
  [
    ((modm, xK_space)                , spawn "rofi -show drun") -- Rofi
  , ((mod1Mask .|. controlMask, xK_t), spawn $ XMonad.terminal conf) -- Terminal
  , ((mod1Mask .|. controlMask, xK_e), spawn "emacs")
  , ((mod1Mask .|. controlMask, xK_f), spawn "firefox")
  , ((modm, xK_s)                    , spawn "sleep 0.2; scrot -s")

  , ((0, xF86XK_AudioRaiseVolume), spawn
      "amixer -D pulse set Master 5%+ unmute; \
      \ $(~/.local/bin/notify-send.sh --replace-file=/tmp/xmon \"$(. ~/.xmonad/vol)\" -u low)")
  , ((0, xF86XK_AudioLowerVolume), spawn
      "amixer -D pulse set Master 5%- unmute; \
      \ $(~/.local/bin/notify-send.sh --replace-file=/tmp/xmon \"$(. ~/.xmonad/vol)\" -u low)")
  , ((0, xF86XK_AudioMute), spawn
      "amixer -D pulse set Master toggle; \
      \ $(~/.local/bin/notify-send.sh --replace-file=/tmp/xmon \"$(. ~/.xmonad/vol)\" -u low)")
  , ((modm, xK_d), spawn
      "$(~/.local/bin/notify-send.sh --replace-file=/tmp/xmon \"$(date '+%A, %d %B %H:%M')\" -u low)")
  -- TODO: Add battery notification and notifications for workspace changes

  , ((modm, xK_w)     , kill) -- Close application
  , ((modm, xK_Tab)   , toggleWS) -- Switch to last workspace
  , ((modm, xK_r)     , sendMessage Rotate) -- Rotate windows
  , ((modm, xK_z)     , sendMessage Swap) -- Swap windows

    -- Directional navigation of windows
  , ((modm, xK_l), windowGo R False)
  , ((modm, xK_h), windowGo L False)
  , ((modm, xK_k), windowGo U False)
  , ((modm, xK_j), windowGo D False)

    -- Expand and shrink windows
  , ((modm .|. controlMask,                xK_l), sendMessage $ ExpandTowards R)
  , ((modm .|. controlMask,                xK_h), sendMessage $ ExpandTowards L)
  , ((modm .|. controlMask,                xK_j), sendMessage $ ExpandTowards D)
  , ((modm .|. controlMask,                xK_k), sendMessage $ ExpandTowards U)
  , ((modm .|. controlMask .|. shiftMask , xK_l), sendMessage $ ShrinkFrom R)
  , ((modm .|. controlMask .|. shiftMask , xK_h), sendMessage $ ShrinkFrom L)
  , ((modm .|. controlMask .|. shiftMask , xK_j), sendMessage $ ShrinkFrom D)
  , ((modm .|. controlMask .|. shiftMask , xK_k), sendMessage $ ShrinkFrom U)
  ]

