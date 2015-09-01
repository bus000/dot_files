import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Actions.Volume
import Data.Map
import Data.Monoid

main = do
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"

    xmonad $ defaultConfig
        { terminal     = "xterm"
        , borderWidth  = 3
        , manageHook   = manageDocks <+> manageHook defaultConfig
        , layoutHook   = avoidStruts  $  layoutHook defaultConfig
        , logHook      = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle  = xmobarColor "green" "" . shorten 50
            }
        , XMonad.keys = XMonad.keys defaultConfig `mappend`
            \c -> fromList [ ((0, xK_F6), toggleMute >> return ())
                           , ((0, xK_F7), setMute False >> lowerVolume 4 >> return ())
                           , ((0, xK_F8), setMute False >> raiseVolume 4 >> return ())
                           ]
        }
