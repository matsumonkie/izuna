module IzunaBuilder.ProjectInfo.Util where

import           System.FilePath.Posix       ((</>))
import qualified System.FilePath.Posix       as FilePath

import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.Type

getProjectPath
  :: NonEmptyString Username
  -> NonEmptyString Repo
  -> NonEmptyString Commit
  -> FilePath
getProjectPath username repo commit =
  FilePath.joinPath [ defaultProjectInfoBaseDir
                    , toString username
                    , toString repo
                    , toString commit
                    ]
  where
    defaultProjectInfoBaseDir :: FilePath
    defaultProjectInfoBaseDir = "./backup"

getJsonPath :: FilePath -> FilePath -> String
getJsonPath projectPath projectRoot = projectPath </> "json" </> projectRoot

getHiePath :: FilePath -> String
getHiePath projectPath = projectPath </> "hie"
