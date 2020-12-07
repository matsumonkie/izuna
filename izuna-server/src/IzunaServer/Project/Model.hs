{-# LANGUAGE DeriveGeneric #-}

module IzunaServer.Project.Model where

-- * imports

-- ** aeson

import qualified Data.Aeson                  as Aeson

-- ** ghc

import           GHC.Generics

-- ** izuna-builder

import           IzunaBuilder.NonEmptyString

-- * data

data Project = Project
    { _project_user       :: NonEmptyString
    , _project_repo       :: NonEmptyString
    , _project_package    :: NonEmptyString
    , _project_commit     :: NonEmptyString
    , _project_publicRepo :: Bool
    }
    deriving (Generic)

-- * json

instance Aeson.ToJSON Project where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("_project_" :: String) }

instance Aeson.FromJSON Project where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("_project_" :: String) }
