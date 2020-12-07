module IzunaServer.Project.App ( saveProjectInfoHandler
                               , getProjectInfoHandler
                               ) where

-- * imports

-- ** aeson

import qualified Data.Aeson                     as Aeson

-- ** servant

import qualified Servant
import           Servant.Multipart              (FileData (..),
                                                 MultipartData (..), Tmp)

-- ** transformers

import qualified Control.Monad                  as Monad
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
import           IzunaBuilder.Type

-- * save project

saveProjectInfoHandler
  :: (IO.MonadIO m)
  => NonEmptyString Username
  -> NonEmptyString Repo
  -> NonEmptyString Package
  -> NonEmptyString Commit
  -> MultipartData Tmp
  -> m ()
saveProjectInfoHandler username repo package commit MultipartData{files} =
  IO.liftIO $
    createDirectory directoryPath >>
    Monad.forM_ files (saveProjectInfo (directoryPath </> toString commit))
  where
    directoryPath :: FilePath
    directoryPath =
      FilePath.joinPath [ defaultProjectInfoBaseDir
                        , toString repo
                        , toString username
                        , toString package
                        ]

    createDirectory :: FilePath -> IO ()
    createDirectory directory =
      Dir.createDirectoryIfMissing True directory

    saveProjectInfo :: FilePath -> FileData Tmp -> IO ()
    saveProjectInfo newFilePath FileData{..} = do
      Dir.copyFile fdPayload newFilePath


-- * get project


getProjectInfoHandler
  :: (IO.MonadIO m, Except.MonadError Servant.ServerError m)
  => NonEmptyString Username
  -> NonEmptyString Repo
  -> NonEmptyString Package
  -> NonEmptyString Commit
  -> m ProjectInfo
getProjectInfoHandler username repo package commit = do
  mProjectInfo <- IO.liftIO $ Aeson.decodeFileStrict' filePath
  case mProjectInfo of
    Nothing          -> Servant.throwError Servant.err404
    Just projectInfo -> return projectInfo
  where
    filePath :: FilePath
    filePath =
      FilePath.joinPath [ defaultProjectInfoBaseDir
                        , toString repo
                        , toString username
                        , toString package
                        , toString commit
                        ]

-- * util

defaultProjectInfoBaseDir :: FilePath
defaultProjectInfoBaseDir = "./backup"
