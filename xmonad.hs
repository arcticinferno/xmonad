--[[
--  -> IMPORTS
--]]
import XMonad

-- -- Util
import XMonad.Util.EZConfig ()
import XMonad.Util.Ungrab ()

--[[ 
--  -> SETTINGS
--]]
xMod = mod4Mask

--[[
--  -> MAIN
--]]
main :: IO ()
main = xmonad def
