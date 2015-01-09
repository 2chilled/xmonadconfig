{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-} 
-- FlexibleInstances and MultiParamTypeClasses are necessary for the LayoutClass instance declaration of Flip.

import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName     -- needed for java stuff
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Util.EZConfig
import qualified Data.Map as M
import Control.Monad
import Data.Monoid
import XMonad.Actions.CopyWindow
import XMonad.Util.WorkspaceCompare
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ResizableTile -- Actions.WindowNavigation is nice too
import XMonad.Layout.IM
import Control.Arrow ((***), second)
import XMonad.Layout.PerWorkspace
import XMonad.Config.Desktop (desktopLayoutModifiers)

 
main = xmonad $ myBaseConfig
    { workspaces = myWorkspaces
    , modMask = myModMask -- use the Windows button as mod
    , startupHook = startupHook myBaseConfig <+> setWMName "LG3D" 
    , manageHook = manageHook myBaseConfig <+> myManageHook <+> manageDocks
    , focusedBorderColor = myFocusedBorderColor
    , borderWidth = myBorderWidth
    , layoutHook = myLayouts
    , mouseBindings = \x -> myMouse x <+> mouseBindings myBaseConfig x
    } `additionalKeysP` myKeys

myBaseConfig = kde4Config

myLayouts = desktopLayoutModifiers $ 
  onWorkspaces [] (tiled ||| Flip tiled ||| Full) $ -- you could use Flip tiled on "left" screens..
  onWorkspaces ["gimp"] gimpLayout defaultLayout 
    where
      tiled         = Tall nmaster delta ratio
      nmaster       = 1
      ratio         = 1/2
      delta         = 3/100
      defaultLayout = tiled ||| Mirror tiled ||| Full
      --gimpLayout = withIM (11/64) (Role "gimp-toolbox") $ ResizableTall 2 (1/118) (11/20) [1] ||| Full
      gimpLayout    = avoidStruts $ withIM (11/64) (Role "gimp-toolbox") defaultLayout
 
myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ className   =? c --> doF (W.shift "1") | c <- webApps]
    , [ className   =? c --> doF (W.shift "2") | c <- ircApps]
    , [ className   =? c --> doF (W.shift "2") | c <- otherNonFloats]
    , [ className   =? c --> doF (W.shift "gimp") | c <- gimp]
    ]
  where myFloats       = ["MPlayer", "Gimp", "plasma-desktop", "Plasma-desktop", "plasma", "Plasma", "krunner", 
                         "yakuake", "Yakuake", "ksplashsimple", "ksplashqml", "ksplashx", "Synapse"]
        myOtherFloats  = ["alsamixer"]
        webApps        = ["Firefox"] -- open on desktop 1
        ircApps        = ["Pidgin", "Skype"] -- open on desktop 2
        otherNonFloats = ["Hamster-time-tracker", "Kmail"] -- open on desktop 2
        gimp           = ["Gimp-2.8"]

myKeys = 
  [
    ("M-C-l",   spawn "qdbus org.freedesktop.ScreenSaver /ScreenSaver Lock")
  , ("M-p",     spawn "synapse")
  , ("M-<F4>",  kill)
  , ("<Print>", spawn "ksnapshot")
  ] <+> mySwitchScreensKeys
    where
      mySwitchScreensKeys = 
        [ 
          (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
           | (key, scr)  <- zip "wer" [1,0,2] -- was [0..] *** change to match your screen order ***
           , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
        ]

myMouse x = M.fromList
  [
    ((myModMask,button2), \w -> focus w >> kill)
  ]

showLancelotViaDbusCommand = "dbus-send --print-reply --dest=org.kde.lancelot /Lancelot org.kde.lancelot.App.showCentered"

myWorkspaces = ["1","2","3","4","5","6","7","8","gimp"]

myFocusedBorderColor = "#FFFF00"

myBorderWidth = 2

myModMask = mod4Mask

-- Not used actually

ewmhCopyWindow :: Event -> X All
ewmhCopyWindow ClientMessageEvent {
                ev_window = w,
                ev_message_type = mt,
                ev_data = 0xffffffff : _
        } = withWindowSet $ \s -> do
     a_d <- getAtom "_NET_WM_DESKTOP"
     when (mt == a_d) $ do
         sort' <- getSortByIndex
         let ws = map W.tag $ sort' $ W.workspaces s
         windows $ foldr ((.) . copyWindow w) id ws
         trace $ show w
     return (All True)
ewmhCopyWindow _ = return (All True)

-- | Flip a layout, compute its 180 degree rotated form.
data Flip l a = Flip (l a) deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Flip l) a where    
  runLayout (W.Workspace i (Flip l) ms) r = (map (second flipRect) *** fmap Flip) `fmap` runLayout (W.Workspace i l ms) (flipRect r)
    where screenWidth = fromIntegral $ rect_width r                                               
          flipRect (Rectangle rx ry rw rh) = Rectangle (screenWidth - rx - fromIntegral rw) ry rw rh    
