{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import Reflex.FLTK

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Control.Monad
import Data.IORef

star :: Int -> Int -> Double -> IO ()
star w h n = do
  flcPushMatrix
  flcTranslate (ByXY (ByX (fromIntegral w / 2)) (ByY (fromIntegral h /2)))
  flcScaleWithY (ByXY (ByX (fromIntegral w / 2)) (ByY (fromIntegral h /2)))
  forM_ [0..n] $ \i -> do
    forM_ [(i+1)..n] $ \j -> do
      let i_vertex :: Double =  2 * pi * i/n + 0.1
          j_vertex :: Double =  2 * pi * j/n + 0.1
      flcBeginLine
      flcVertex (ByXY (ByX $ cos i_vertex) (ByY $ sin i_vertex))
      flcVertex (ByXY (ByX $ cos j_vertex) (ByY $ sin j_vertex))
      flcEndLine
  flcPopMatrix

sliderCb :: IORef (Double,Double) -> (Double -> (Double,Double) -> (Double,Double))-> Ref HorSlider -> IO ()
sliderCb sides' sidesf' slider' = do
  v' <- getValue slider'
  modifyIORef sides' (sidesf' v')
  (Just p') <- getParent slider'
  redraw p'

badDraw :: IORef (Double,Double) -> Int -> Int -> ((Double,Double) -> Double) -> IO ()
badDraw sides w h which' = do
  flcSetColor blackColor >> flcRectf (toRectangle (0,0,w,h))
  flcSetColor whiteColor >> readIORef sides >>= star w h . which'


drawWindow :: IORef (Double, Double) ->
              ((Double, Double) -> Double) ->
              Ref Window ->
              IO ()
drawWindow sides' whichf' w' = do
  ww' <- getW w'
  wh' <- getH w'
  badDraw sides' ww' wh' whichf'
  c' <- getChild w' (0 :: Int)
  maybe (return ()) (drawChild w') (c' :: Maybe (Ref Widget))

ui :: MonadAppHost t m => FLTK m ()
ui = do
  visual' <- liftIO $ FL.visual ModeDouble
  liftIO $ if (not visual') then print "Xdbe not supported, faking double buffer with pixmaps.\n" else return ()
  sides' <- liftIO $ newIORef (20,20)
  w01 <- liftIO $ windowNew (toSize (420,420)) Nothing (Just "Fl_Single_Window")
  liftIO $ setBox w01 FlatBox
  w1 <- within w01 $ do
    w1 <- liftIO $ singleWindowCustom
            (Size (Width 400) (Height 400))
            (Just (Position (X 10) (Y 10)))
            (Just "Single Window")
            (Just (\w -> drawWindow sides' fst (safeCast w)))
            defaultCustomWidgetFuncs
            defaultCustomWindowFuncs
    liftIO $ setBox w1 FlatBox
    liftIO $ setColor w1 blackColor
    liftIO $ setResizable w1 (Just w1)
    within w1 $ do
      slider0 <- liftIO $ horSliderNew (toRectangle (20,370,360,25)) Nothing
      liftIO $ range slider0 2 30
      liftIO $ setStep slider0 1
      _ <- liftIO $ readIORef sides' >>= setValue slider0 . fst
      liftIO $ setCallback slider0 (sliderCb sides' (\v (_,s2) -> (v, s2)))
    return w1
  w02 <- liftIO $ windowNew (Size (Width 420) (Height 420)) Nothing (Just "Fl_Double_Window")
  liftIO $ setBox w02 FlatBox
  w2 <- within w02 $ do
    w2 <- liftIO $ doubleWindowCustom
            (Size (Width 400) (Height 400))
            (Just $ Position (X 10) (Y 10))
            (Just "Fl_Double_Window")
            (Just (\w -> drawWindow sides' snd (safeCast w)))
            defaultCustomWidgetFuncs
            defaultCustomWindowFuncs
    liftIO $ setBox w2 FlatBox
    liftIO $ setColor w2 blackColor
    liftIO $ setResizable w2 (Just w2)
    within w2 $ do
      slider1 <- liftIO $ horSliderNew (toRectangle $ (20,370,360,25)) Nothing
      liftIO $ range slider1 2 30
      liftIO $ setStep slider1 1
      _ <- liftIO $ readIORef sides' >>= setValue slider1 . fst
      liftIO $ setCallback slider1 (sliderCb sides' (\v (s1,_) -> (s1,v)))
    return w2
  liftIO $ showWidget w01
  liftIO $ showWidget w1
  liftIO $ showWidget w02
  liftIO $ showWidget w2
  return ()

main :: IO ()
main = runFLTKIO ui
