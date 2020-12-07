{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}

module IzunaBuilder.App(run) where

import           Options.Applicative            (execParser)
import qualified Say

import           IzunaBuilder.BuilderConfig.App
import           IzunaBuilder.ProjectInfo.App
import           IzunaBuilder.ProjectInfo.Model

-- * run

run :: IO ProjectInfo
run = do
  Say.sayString "running izuna-builder!"
  execParser parserInfo >>= getProjectInfo
