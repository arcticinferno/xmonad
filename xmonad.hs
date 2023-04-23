import XMonad

-- General
import Data.Maybe (isJust)

-- Qualified
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Util
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Ungrab ()

import XMonad.Layout.NoBorders (smartBorders)

-- Layout
import XMonad.Layout.Spacing

-- System
import System.IO

type Keybind = (String, X ())

(<:>) :: String -> String -> Keybind
(<:>) x y = (x, spawn y)

xMod :: KeyMask
xMod = mod1Mask -- Alt

xBrowser :: Keybind
xBrowser = "M-u" <:> "firefox-bin"

xScreenshot :: Keybind
xScreenshot = "M-h" <:> "flameshot gui"

xTerminal :: Keybind
xTerminal = "M-S-<Return>" <:> "alacritty"

xLauncher :: Keybind
xLauncher = "M-p" <:> "rofi -show drun"

switchLang :: Keybind
switchLang = "M-v" <:> "~/.local/bin/switchlang"

xMusic :: Keybind
xMusic = "M-S-u" <:> "alacritty -e \"ncmpcpp\""

xDoc :: Keybind
xDoc =
  "M-S-f" <:>
  "zathura ~/doc/$(/bin/ls ~/doc | rofi -dmenu -p \"Open: \") || return"

xEditFile :: Keybind
xEditFile = "M-f" <:> "~/.local/bin/fx"

xLayout =
  avoidStruts $
  smartBorders $
  spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $
  layoutHook def

main = do
  xmproc0 <- spawnPipe "xmobar -x 0 ~/.xmobarrc -d"
  xmproc1 <- spawnPipe "xmobar -x 1 ~/.xmobarrc -d"
  xmproc2 <- spawnPipe "xmobar -x 2 ~/.xmobarrc -d"
  xmonad $
    docks $
    def
      { terminal = "alacritty"
      , borderWidth = 2
      , normalBorderColor = "#cccccc"
      , workspaces =
          [ " λ-dev "
          , " λ-sys "
          , " λ-music "
          , " λ-www "
          , " λ-vid "
          , " λ-doc "
          , " λ-mvs "
          , " λ-kvm "
          , " λ-game "
          ]
      , focusedBorderColor = "#cd8b00"
      , manageHook = manageDocks <+> manageHook def
      , layoutHook = avoidStruts $ xLayout -- Add spacing with 10 pixels
      , logHook =
          do let ppOutput' x =
                   hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x >>
                   hPutStrLn xmproc2 x
             dynamicLogWithPP
               xmobarPP
                 { ppOutput = ppOutput'
                 , ppTitle = xmobarColor "green" "" . shorten 50
                 , ppCurrent = xmobarColor "#ff6b6b" "" . id
                 , ppVisible = xmobarColor "#ff9ff3" "" . id
                 , ppHidden = xmobarColor "#80c8ff" "" . id
                 , ppHiddenNoWindows = xmobarColor "#80c8ff" "" . id
                 , ppSep = "   "
                 , ppWsSep = " "
                 }
      } `additionalKeysP`
    [ xBrowser
    , xLauncher
    , xTerminal
    , xMusic
    , xDoc
    , xScreenshot
    , xEditFile
    , switchLang
    ]
