module ProjectInfo.App ( saveProjectInfoHandler
                       , getProjectInfoHandler
                       )where

-- * imports

-- ** aeson

import qualified Data.Aeson             as Aeson

-- ** servant

import qualified Servant

-- ** transformers

import qualified Control.Monad.Except   as Except
import qualified Control.Monad.IO.Class as IO

-- ** filepath

import           System.FilePath.Posix  ((</>))
import qualified System.FilePath.Posix  as FilePath

-- ** directory

import qualified System.Directory       as Dir

-- ** local

import           ModuleAst.Model


-- * save project

saveProjectInfoHandler
  :: (IO.MonadIO m)
  => String
  -> String
  -> String
  -> ModulesAst
  -> m ()
saveProjectInfoHandler repo user commit modulesAst =
  IO.liftIO $ createDirectory directoryPath >> saveProjectInfo (directoryPath </> commit) modulesAst
  where
    directoryPath :: FilePath
    directoryPath =
      FilePath.joinPath [defaultProjectInfoBaseDir, repo, user]

    createDirectory :: FilePath -> IO ()
    createDirectory directory =
      Dir.createDirectoryIfMissing True directory

    saveProjectInfo :: FilePath -> ModulesAst -> IO ()
    saveProjectInfo commit modulesAst =
      Aeson.encodeFile commit modulesAst

-- * get project

getProjectInfoHandler
  :: (IO.MonadIO m, Except.MonadError Servant.ServerError m)
  => String
  -> String
  -> String
  -> m ModulesAst
getProjectInfoHandler repo user commit = do
  mProjectInfo <- IO.liftIO $ Aeson.decodeFileStrict' filePath
  case mProjectInfo of
    Nothing          -> Servant.throwError Servant.err404
    Just projectInfo -> return projectInfo
  where
    filePath :: FilePath
    filePath =
      FilePath.joinPath [defaultProjectInfoBaseDir, repo, user, commit]

-- * util

defaultProjectInfoBaseDir :: FilePath
defaultProjectInfoBaseDir = "./backup"
