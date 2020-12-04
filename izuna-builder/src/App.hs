{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module App(run) where

import           Options.Applicative (execParser)
import qualified Say

import           BuilderConfig.App
import           ModuleAst.App
import           ModuleAst.Model

-- * run

run :: IO ProjectInfo
run = do
  Say.sayString "running izuna-builder!"
  execParser parserInfo >>= getProjectInfo
