module Main where

import qualified Data.Aeson                     as Aeson
import           Data.Function                  ((&))
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
      [ toString _projectInfo_user
      , toString _projectInfo_repo
      , toString _projectInfo_package
      , toString _projectInfo_commit
      ] & List.intercalate "-"
