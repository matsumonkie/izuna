module IzunaServer.Project.App ( saveProjectInfoHandler
                               , getProjectInfoHandler
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

-- ** directory

import qualified System.Directory               as Dir

-- ** local

import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.ProjectInfo.Model
import           IzunaServer.Project.Model

-- * save project

saveProjectInfoHandler
  :: (IO.MonadIO m)
  => ProjectInfo
  -> m ()
saveProjectInfoHandler projectInfo@ProjectInfo{..} =
  IO.liftIO $ createDirectory directoryPath >> saveProjectInfo (directoryPath </> toString _projectInfo_commit) projectInfo
  where
    directoryPath :: FilePath
    directoryPath =
      FilePath.joinPath [ defaultProjectInfoBaseDir
                        , toString _projectInfo_repo
                        , toString _projectInfo_user
                        ]

    createDirectory :: FilePath -> IO ()
    createDirectory directory =
      Dir.createDirectoryIfMissing True directory

    saveProjectInfo :: FilePath -> ProjectInfo -> IO ()
    saveProjectInfo filepath projectInfo =
      Aeson.encodeFile filepath projectInfo


-- * get project


getProjectInfoHandler
  :: (IO.MonadIO m, Except.MonadError Servant.ServerError m)
  => Project
  -> m ProjectInfo
getProjectInfoHandler Project{..} = do
  mProjectInfo <- IO.liftIO $ Aeson.decodeFileStrict' filePath
  case mProjectInfo of
    Nothing          -> Servant.throwError Servant.err404
    Just projectInfo -> return projectInfo
  where
    filePath :: FilePath
    filePath =
      FilePath.joinPath [ defaultProjectInfoBaseDir
                        , toString _project_repo
                        , toString _project_user
                        , toString _project_commit
                        ]

-- * util

defaultProjectInfoBaseDir :: FilePath
defaultProjectInfoBaseDir = "./backup"
