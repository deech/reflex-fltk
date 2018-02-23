{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Lib where

import qualified Graphics.UI.FLTK.LowLevel.FL as FL

newtype Widget a = Widget { unWidget :: IO a } deriving (Functor, Applicative, Monad)

runWidget :: Widget () -> IO ()
runWidget (Widget a) = a >> FL.run >> FL.flush

runWidgetRepl :: Widget () -> IO ()
runWidgetRepl (Widget a) = a >> FL.replRun
