import Control.Monad
import XMonad
import XMonad.Actions.MouseGestures
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig(additionalKeys)

import qualified XMonad.StackSet as W

--gestures = M.fromList
	-- [ ([], focus)
	--, ( [L], ) -- previous desktop
	--, ( [R], ) -- next desktop
	--, ( [D], ) -- new term window
	--, ( [D,U], ) -- new window as the one bellow
	--, ( [U,D], ) -- reload haskell config
	--, ( [D,R], ) -- close window below
	-- ]

main = xmonad =<< xmobar myConfig

myConfig = defaultConfig { manageHook = manageDocks <+> manageHook defaultConfig
		, layoutHook = avoidStruts $ layoutHook defaultConfig
		, logHook = dynamicLog
		, modMask = mod4Mask
		, terminal = "urxvt"
		, borderWidth = 0
		} `additionalKeys`
		[ ( (mod4Mask, xK_g), spawn "chromium"),
		  ( (mod4Mask, xK_f), spawn "firefox"),
		  ( (mod4Mask, xK_o), spawn "opera"),
		  ( (mod4Mask, xK_t), spawn "urxvt -e tmux"),
		  ( (mod4Mask, xK_p), spawn "scrot"),
		  ( (mod4Mask, xK_s), spawn "skype"),
		  ( (mod4Mask, xK_e), spawn "eclipse") ]
