--[[
--  -> IMPORTS
--]]
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

--  -> SETTINGS
--]]
--
type Keybind = (String, X ())

(<:>) :: String -> String -> Keybind
(<:>) x y = (x, spawn y)

xMod :: KeyMask
xMod = mod1Mask -- Alt

xBrowser :: Keybind
xBrowser = "M-u" <:> "firefox-bin"

xTerminal :: Keybind
xTerminal = "M-S-<Return>" <:> "alacritty"

xLauncher :: Keybind
xLauncher = "M-p" <:> "rofi -show drun"

xMusic :: Keybind
xMusic = "M-S-u" <:> "alacritty -e \"ncmpcpp\""

xLayout =
  avoidStruts $
  smartBorders $
  spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $
  layoutHook def

--[[
--  -> MAIN
--]]
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
      , focusedBorderColor = "#cd8b00"
      , manageHook = manageDocks <+> manageHook def
      , layoutHook = avoidStruts $ xLayout -- Add spacing with 10 pixels
      , logHook =
          dynamicLogWithPP
            xmobarPP
              { ppOutput =
                  \x ->
                    hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x >>
                    hPutStrLn xmproc2 x
              , ppTitle = xmobarColor "green" "" . shorten 50
              , ppCurrent = xmobarColor "#429942" "" . wrap "[" "]"
              , ppVisible = xmobarColor "#5b6ea7" "" . wrap "(" ")"
              , ppHidden = xmobarColor "#4c7899" ""
              , ppHiddenNoWindows = xmobarColor "#4c7899" "" . wrap "-" "-"
              , ppSep = "   "
              , ppWsSep = " "
              }
      } `additionalKeysP`
    [xBrowser, xLauncher, xTerminal, xMusic]
