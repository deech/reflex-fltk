{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances, GADTs, FlexibleContexts #-}

module Reflex.FLTK(module Reflex.FLTK, module Control.Monad.IO.Class, module Reflex.Host.App, module Data.Default) where

import Data.Default
import Reflex.Class
import Reflex.Host.Class
import Reflex.Host.App
import Data.Text(pack, Text)
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Concurrent(forkIO)
import Reflex.Spider
import qualified Reflex.Spider.Internal(Spider, runSpiderHost)
import qualified Data.DList as DL
import qualified Graphics.UI.FLTK.LowLevel.FL as FL

newtype FLTK m a = FLTK { unFLTK :: m a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

instance MonadTrans FLTK where
  lift = FLTK

instance MonadSample t m => MonadSample t (FLTK m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (FLTK m) where
  headE = lift . headE
  buildDynamic p e = lift $ buildDynamic p e

runFLTKAux :: MonadAppHost t m => IO () -> FLTK m () -> m ()
runFLTKAux run (FLTK a) = do
  a
  (quit, quitTrigger) <- newExternalEvent
  performPostBuild_ $ return $ infoQuit $ DL.singleton quit
  liftIO $ forkIO $ run >> quitTrigger () >> return ()
  return ()

runFLTK :: MonadAppHost t m => FLTK m () -> m ()
runFLTK = runFLTKAux (FL.run >> FL.flush)

runFLTKIO :: FLTK (AppHost (SpiderTimeline Reflex.Spider.Global)) () -> IO ()
runFLTKIO = runSpiderHost . hostApp . runFLTK

runFLTKRepl :: MonadAppHost t m => FLTK m () -> m ()
runFLTKRepl = runFLTKAux FL.replRun

data ButtonAttr = ButtonAttr

instance Default ButtonAttr where
  def = ButtonAttr

button :: MonadAppHost t m => Rectangle -> Dynamic t Text -> ButtonAttr -> FLTK m (Event t ())
button rect label _ = FLTK $ do
  (e, trigger) <- newExternalEvent
  initLabel <- sample $ current label
  b' <- liftIO $ buttonNew rect $ Just initLabel
  liftIO $ setLabelsize b' (FontSize 10)
  liftIO $ setCallback b' (const $ trigger () >> return ())
  performEvent_ $ fmap (liftIO . setLabel b') $ updated label
  return e

within :: (Match br ~ FindOp window window (Begin ()), Op (Begin ()) br window (IO ()),
           Match er ~ FindOp window window (End ()), Op (End ()) er window (IO ()), MonadIO m) =>
          Ref window -> FLTK m a -> FLTK m a
within window x = do
  () <- liftIO $ begin window
  res <- x
  () <- liftIO $ end window
  return res
