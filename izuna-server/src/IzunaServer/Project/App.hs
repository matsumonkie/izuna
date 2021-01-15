module IzunaServer.Project.App ( getProjectInfoHandler
                               ) where

-- * imports

-- ** aeson

import qualified Data.Aeson                     as Aeson

-- ** base

import qualified Data.Foldable                  as Foldable
import           Data.Functor                   ((<&>))
import qualified Data.Traversable               as T

-- ** containers

import qualified Data.Map                       as Map

-- ** directory

import qualified System.Directory               as Dir

-- ** servant

import qualified Servant

-- ** transformers

import qualified Control.Monad.Except           as Except
import qualified Control.Monad.IO.Class         as IO

-- ** local

import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.ProjectInfo.Model
import           IzunaBuilder.ProjectInfo.Util
import           IzunaBuilder.Type

-- * get project


getProjectInfoHandler
  :: (IO.MonadIO m, Except.MonadError Servant.ServerError m)
  => NonEmptyString Username
  -> NonEmptyString Repo
  -> NonEmptyString Commit
  -> [String]
  -> m ModulesInfo
getProjectInfoHandler username repo commit files = do
  allFileExist <- IO.liftIO $ T.for files (Dir.doesFileExist . getFilePath) <&> and
  case allFileExist of
    False -> Servant.throwError Servant.err404
    True -> do
      eFilesInfo <- IO.liftIO $
        T.for files (\file ->
                       Aeson.decodeFileStrict' (getFilePath file) <&> (\d -> (file, d))
                    ) <&> checkDecodeError
      case eFilesInfo of
        Left errors -> do
          IO.liftIO $ putStrLn $ "For project: " <> projectPath <> " - could not decode file(s): " <> show errors
          Servant.throwError Servant.err500
        Right filesInfo ->
          return $ Map.fromList filesInfo
  where
    projectPath :: FilePath
    projectPath = getProjectPath username repo commit

    getFilePath :: FilePath -> FilePath
    getFilePath file =
      getJsonPath projectPath file

checkDecodeError :: [ (FilePath, Maybe ModuleInfo) ] -> Either [FilePath] [ (FilePath, ModuleInfo) ]
checkDecodeError filesInfo = do
    Foldable.foldl' go (Right []) filesInfo
  where
    go :: Either [FilePath] [ (FilePath, ModuleInfo) ] -> (FilePath, Maybe ModuleInfo) -> Either [FilePath] [ (FilePath, ModuleInfo) ]
    go acc (filePath, mModuleInfo) =
      case (acc, mModuleInfo) of
        (Left _, Just _) -> acc
        (Left errors, Nothing) -> Left (filePath : errors)
        (Right _, Nothing) -> Left [filePath]
        (Right valid, Just moduleInfo) -> Right ((filePath, moduleInfo): valid)
