module ProjectInfo.AppSpec where

import           Data.Function                  ((&))
import qualified Data.List.NonEmpty             as NE
import qualified Data.Map                       as Map
import qualified Data.Maybe                     as Maybe
import           Data.Text                      (Text)
import           Test.Hspec

import           IzunaBuilder.BuilderConfig.App
import           IzunaBuilder.NonEmptyString
import qualified IzunaBuilder.ProjectInfo.App   as App
import           IzunaBuilder.ProjectInfo.Model

spec :: Spec
spec =
  describe "parse hie files to ast" $ do
    it "0" $ do
      fileContent <- getModuleAst "./izuna-builder/test/fixtures/hie/project0" "src/Lib.hs"

      fileContent `shouldBe` ["module Lib"
                             ,"    ( someFunc"
                             ,"    ) where"
                             ,""
                             ,"someFunc :: IO ()"
                             ,"someFunc = <span data-specialized-type='String -> IO ()'>putStrLn</span> <span data-specialized-type='String'>\"someFunc\"</span>"]

    it "1" $ do
      fileContent <- getModuleAst "./izuna-builder/test/fixtures/hie/project1" "src/Lib.hs"
      fileContent `shouldBe` ["module Lib"
                             ,"    ( someFunc"
                             ,"    ) where"
                             ,""
                             ,"import           Data.Foldable (foldl')"
                             ,""
                             ,"someFunc :: IO ()"
                             ,"someFunc = <span data-specialized-type='String -> IO ()'>putStrLn</span> <span data-specialized-type='String'>\"someFunc\"</span>"
                             ,""
                             ,"add :: Int -> Int -> Int"
                             ,"add <span data-specialized-type='Int'>x</span> <span data-specialized-type='Int'>y</span> ="
                             ,"  <span data-specialized-type='Int'>x</span> <span data-specialized-type='Int -> Int -> Int'>+</span> <span data-specialized-type='Int'>y</span>"
                             ,""
                             ,"sum :: [Int] -> Int"
                             ,"sum <span data-specialized-type='[Int]'>xs</span> ="
                             ,"  <span data-specialized-type='(Int -> Int -> Int) -> Int -> [Int] -> Int'>foldl'</span> <span data-specialized-type='Int -> Int -> Int'>add</span> <span data-specialized-type='Int'>0</span> <span data-specialized-type='[Int]'>xs</span>"
                             ,""
                             ,"foo :: String"
                             ,"foo ="
                             ,"  let x = <span data-specialized-type='Integer'>42</span>"
                             ,"  in <span data-specialized-type='Integer -> String'>show</span> <span data-specialized-type='Integer'>x</span>"]


getModuleAst :: FilePath -> FilePath -> IO [Text]
getModuleAst filePath moduleName = do
  ProjectInfo { _projectInfo_modulesInfo } <- App.getProjectInfo emptyBuilderConfig { _builderConfig_hieDirectory = filePath }
  _projectInfo_modulesInfo &
    Map.lookup moduleName &
    Maybe.fromMaybe emptyModuleInfo &
    _minfo_fileContent &
    return
  where
    emptyModuleInfo :: ModuleInfo
    emptyModuleInfo = ModuleInfo
      { _minfo_asts = []
      , _minfo_fileContent = []
      }

    emptyBuilderConfig :: BuilderConfig
    emptyBuilderConfig =
      BuilderConfig { _builderConfig_hieDirectory = ""
                    , _builderConfig_user         = NonEmptyString $ NE.fromList "user"
                    , _builderConfig_repo         = NonEmptyString $ NE.fromList "repo"
                    , _builderConfig_package      = NonEmptyString $ NE.fromList "package"
                    , _builderConfig_commit       = NonEmptyString $ NE.fromList "commit"
                    , _builderConfig_publicRepo   = True
                    }
