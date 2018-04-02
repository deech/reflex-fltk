{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.FLTK

import Reflex.Host.App
import Reflex.Spider
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.FLTKHS

ui :: MonadAppHost t m => FLTK m ()
ui = do
  w1 <- liftIO $ doubleWindowNew (Size (Width 220) (Height 220)) Nothing (Just "clock")
  within w1 $ do
    c1 <- liftIO $ clockNew (toRectangle (0,0,220,220)) Nothing
    liftIO $ setResizable w1 (Just c1)
  w2 <- liftIO $ doubleWindowNew (Size (Width 220) (Height 220)) Nothing (Just "Rounded Clock")
  within w2 $ do
    c2 <- liftIO $ clockNew (toRectangle (0,0,220,220)) Nothing
    liftIO $ setType c2 RoundClock
    liftIO $ setResizable w2 (Just c2)
  liftIO $ showWidget w1
  liftIO $ showWidget w2
  return ()

main :: IO ()
main = runFLTKIO ui
