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
  liftIO $ begin w1
  c1 <- liftIO $ clockNew (toRectangle (0,0,220,220)) Nothing
  liftIO $ setResizable w1 (Just c1)
  liftIO $ end w1
  w2 <- liftIO $ doubleWindowNew (Size (Width 220) (Height 220)) Nothing (Just "Rounded Clock")
  liftIO $ begin w2
  c2 <- liftIO $ clockNew (toRectangle (0,0,220,220)) Nothing
  liftIO $ setType c2 RoundClock
  liftIO $ setResizable w2 (Just c2)
  liftIO $ end w2
  liftIO $ setXclass w1 "Fl_Clock"
  liftIO $ setXclass w2 "Fl_Clock"
  liftIO $ showWidget w1
  liftIO $ showWidget w2
  return ()

main :: IO ()
main = runSpiderHost $ hostApp $ runFLTK ui
