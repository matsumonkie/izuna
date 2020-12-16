{-# LANGUAGE DeriveGeneric #-}

module IzunaServer.Env
  ( getEnv
  , Env(..)
  ) where

import qualified Dhall
import           GHC.Generics      (Generic)

import           IzunaBuilder.Type

data Env = Env
    { _env_githubAuthToken :: Text
    , _env_port            :: Nat
    }
    deriving (Generic, Show)

instance Dhall.FromDhall Env where
  autoWith _ = Dhall.record $
    Env
      <$> Dhall.field "githubAuthToken" Dhall.auto
      <*> Dhall.field "appPort" Dhall.auto

getEnv :: IO Env
getEnv =
  Dhall.input Dhall.auto "./server.dhall"
