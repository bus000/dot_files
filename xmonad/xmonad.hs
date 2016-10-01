import qualified Data.Map as Map
import Data.Monoid
import System.IO
import XMonad
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)

main = do
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"

    xmonad $ def
        { terminal     = "xterm"
        , borderWidth  = 3
        , normalBorderColor = "#222222"
        , focusedBorderColor = "#222222"
        , manageHook   = manageDocks <+> manageHook def
        , layoutHook   = avoidStruts  $  layoutHook def
        , workspaces = ["www", "code"] ++ map show [3..9]
        , logHook      = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle  = xmobarColor "green" "" . shorten 50
            }
        , XMonad.keys = XMonad.keys def `mappend`
            \c -> Map.fromList [ ((0, xK_F6), toggleMute >> return ())
                           , ((0, xK_F7), setMute False >> lowerVolume 4 >> return ())
                           , ((0, xK_F8), setMute False >> raiseVolume 4 >> return ())
                           ]
        }
