module IzunaServer.Project.App ( getProjectInfoHandler
                               ) where

-- * imports

-- ** aeson

import qualified Data.Aeson                     as Aeson

-- ** servant

import qualified Servant

-- ** transformers

import qualified Control.Monad.Except           as Except
import qualified Control.Monad.IO.Class         as IO

-- ** filepath

import           System.FilePath.Posix          ((</>))
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
  mProjectInfo <- IO.liftIO $ Aeson.decodeFileStrict' (filePath </> "json")
  case mProjectInfo of
    Nothing          -> Servant.throwError Servant.err404
    Just projectInfo -> return projectInfo
  where
    filePath :: FilePath
    filePath =
      FilePath.joinPath [ defaultProjectInfoBaseDir
                        , toString username
                        , toString repo
                        , toString package
                        , toString commit
                        ]

-- * util

defaultProjectInfoBaseDir :: FilePath
defaultProjectInfoBaseDir = "./backup"
