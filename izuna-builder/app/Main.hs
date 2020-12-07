module Main where

import qualified Data.Aeson                     as Aeson
import           Data.Function                  ((&))
import           Data.Functor                   ((<&>))
import qualified Data.List                      as List

import           IzunaBuilder.App
import           IzunaBuilder.NonEmptyString    (toString)
import           IzunaBuilder.ProjectInfo.Model

main :: IO ()
main = do
  run >>= \projectInfo -> Aeson.encodeFile (filename projectInfo) projectInfo
  where
    filename :: ProjectInfo -> FilePath
    filename ProjectInfo{..} =
      [ _projectInfo_user
      , _projectInfo_repo
      , _projectInfo_package
      , _projectInfo_commit
      ] <&> toString & List.intercalate "-"
