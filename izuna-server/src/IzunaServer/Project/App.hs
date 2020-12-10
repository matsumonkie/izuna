module IzunaServer.Project.App ( getProjectInfoHandler
                               ) where

-- * imports

-- ** aeson

import qualified Data.Aeson                     as Aeson

-- ** directory

import qualified System.Directory               as Dir

-- ** servant

import qualified Servant

-- ** transformers

import qualified Control.Monad.Except           as Except
import qualified Control.Monad.IO.Class         as IO

-- ** filepath

import qualified System.FilePath.Posix          as FilePath

-- ** local

import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.ProjectInfo.Model
import           IzunaBuilder.Type

-- * get project


getProjectInfoHandler
  :: (IO.MonadIO m, Except.MonadError Servant.ServerError m)
  => NonEmptyString Username
  -> NonEmptyString Repo
  -> NonEmptyString Package
  -> NonEmptyString Commit
  -> m ModulesInfo
getProjectInfoHandler username repo package commit = do
  fileExists <- IO.liftIO $ Dir.doesFileExist filePath
  case fileExists of
    False -> Servant.throwError Servant.err404
    True -> do
      mProjectInfo <- IO.liftIO $ Aeson.decodeFileStrict' filePath
      case mProjectInfo of
        Nothing          -> do
          IO.liftIO $ putStrLn $ "could not decode file to json for project: " <> filePath
          Servant.throwError Servant.err500
        Just projectInfo -> return projectInfo
  where
    filePath :: FilePath
    filePath =
      FilePath.joinPath [ defaultProjectInfoBaseDir
                        , toString username
                        , toString repo
                        , toString package
                        , toString commit
                        , "json"
                        ]

-- * util

defaultProjectInfoBaseDir :: FilePath
defaultProjectInfoBaseDir = "./backup"
