{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}

module App(run) where

import           Options.Applicative (execParser)
import qualified Say

import           BuilderConfig.App
import           ProjectInfo.App
import           ProjectInfo.Model

-- * run

run :: IO ProjectInfo
run = do
  Say.sayString "running izuna-builder!"
  execParser parserInfo >>= getProjectInfo
