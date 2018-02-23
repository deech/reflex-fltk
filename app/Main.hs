{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS

buttonCb :: Ref Button -> FLTK ()
buttonCb b' = FLTK $ do
  l' <- getLabel b'
  if (l' == "Hello world")
    then setLabel b' "Goodbye world"
    else setLabel b' "Hello world"

ui :: FLTK ()
ui = FLTK $ do
 window <- windowNew
           (Size (Width 115) (Height 100))
           Nothing
           Nothing
 begin window
 b' <- buttonNew
        (Rectangle (Position (X 10) (Y 30)) (Size (Width 95) (Height 30)))
        (Just "Hello world")
 setLabelsize b' (FontSize 10)
 setCallback b' (unFLTK . buttonCb)
 end window
 showWidget window

main :: IO ()
main = runFLTK ui
