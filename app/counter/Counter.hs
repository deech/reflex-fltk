{-# LANGUAGE RecursiveDo #-}
module Main where

import Reflex.FLTK

import Graphics.UI.FLTK.LowLevel.FLTKHS
import Reflex.Class
import Reflex.Host.App
import Reflex.Host.Class
import Reflex.Spider
import Reflex.Dynamic
import Data.Text(Text, pack)
import Control.Concurrent (forkIO)
import Control.Monad(forever)
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Data.DList as DL
  
ui :: MonadAppHost t m => FLTK m ()
ui = mdo
  window <- liftIO $ windowNew
            (Size (Width 115) (Height 100))
            Nothing
            Nothing
  liftIO $ begin window
  label <- foldDyn (const (+1)) 0 b
  b <- button
         (Rectangle (Position (X 10) (Y 30)) (Size (Width 95) (Height 30)))
         (fmap (pack . show) label)
  liftIO $ end window
  liftIO $ showWidget window
  return ()

main :: IO ()
main = runSpiderHost $ hostApp $ runFLTK ui
