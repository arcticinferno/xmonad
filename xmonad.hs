--[[
--  -> IMPORTS
--]]
import XMonad

-- -- Util
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Ungrab ()

--[[ 
--  -> SETTINGS
--]]
xMod :: KeyMask
xMod = mod1Mask -- Alt

xEditor :: String
xEditor = "emacsclient -c -a 'emacs'"

xBrowser :: String
xBrowser = "firefox"

xTerminal :: String
xTerminal = "alacritty"

xConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
xConfig =
  def {modMask = xMod} `additionalKeysP`
  [ ("M-S-u", spawn xEditor)
  , ("M-u", spawn xBrowser)
  , ("M-S-<Return>", spawn xTerminal)
  ]

--[[
--  -> MAIN
--]]
main :: IO ()
main = xmonad def
