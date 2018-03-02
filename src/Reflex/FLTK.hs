{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}

module Reflex.FLTK where

import Reflex.Class
import Reflex.Host.Class
import Reflex.Host.App
import Data.Text(pack, Text)
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Concurrent(forkIO)
import qualified Reflex.Spider.Internal(Spider, runSpiderHost)
import qualified Data.DList as DL
import qualified Graphics.UI.FLTK.LowLevel.FL as FL

newtype FLTK m a = FLTK { unFLTK :: m a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

runFLTKAux :: MonadAppHost t m => IO () -> FLTK m () -> m ()
runFLTKAux run (FLTK a) = do
  a
  (quit, quitTrigger) <- newExternalEvent
  performPostBuild_ $ return $ infoQuit $ DL.singleton quit
  liftIO $ forkIO $ run >> quitTrigger () >> return ()
  return ()

runFLTK :: MonadAppHost t m => FLTK m () -> m ()
runFLTK = runFLTKAux (FL.run >> FL.flush)

runFLTKRepl :: MonadAppHost t m => FLTK m () -> m ()
runFLTKRepl = runFLTKAux FL.replRun

button :: MonadAppHost t m => Rectangle -> Dynamic t Text -> FLTK m (Event t ())
button rect label = FLTK $ do
  (e, trigger) <- newExternalEvent
  initLabel <- sample $ current label
  b' <- liftIO $ buttonNew rect $ Just initLabel
  liftIO $ setLabelsize b' (FontSize 10)
  liftIO $ setCallback b' (const $ trigger () >> return ())
  performEvent_ $ fmap (liftIO . setLabel b') $ updated label
  return e
