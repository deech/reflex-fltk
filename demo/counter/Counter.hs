{-# LANGUAGE RecursiveDo, GADTs, FlexibleContexts #-}
module Main where

import Reflex.FLTK

import Graphics.UI.FLTK.LowLevel.FLTKHS
import Reflex.Dynamic
import Data.Text(pack)

ui :: MonadAppHost t m => FLTK m ()
ui = mdo
  window <- liftIO $ windowNew
            (Size (Width 115) (Height 100))
            Nothing
            Nothing
  within window $ mdo
    label <- foldDyn (const (+1)) 0 b
    b <- button
         (Rectangle (Position (X 10) (Y 30)) (Size (Width 95) (Height 30)))
         (fmap (pack . show) label)
    return ()
  liftIO $ showWidget window
  return ()

main :: IO ()
main = runFLTKIO ui
