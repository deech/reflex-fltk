{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Lib where

import qualified Graphics.UI.FLTK.LowLevel.FL as FL

newtype FLTK a = FLTK { unFLTK :: IO a } deriving (Functor, Applicative, Monad)

runFLTK :: FLTK () -> IO ()
runFLTK (FLTK a) = a >> FL.run >> FL.flush

runFLTKRepl :: FLTK () -> IO ()
runFLTKRepl (FLTK a) = a >> FL.replRun
