module ProjectInfo.AppSpec where

import           Data.Function                        ((&))
import qualified Data.Map                             as Map
import qualified Data.Maybe                           as Maybe
import           Test.Hspec

import qualified IzunaBuilder.ProjectInfo.App         as App
import           IzunaBuilder.ProjectInfo.Model
import qualified IzunaBuilder.ProjectInfo.RecoverType as RecoverType

spec :: Spec
spec = do
  describe "parse hie files to ast" $ do
    it "project 0" $ do
      ModuleInfo{..} <- getProjectInfo "./izuna-builder/test/fixtures/hie/project0" "src/Lib.hs"
      _minfo_types `shouldBe` Map.fromList [(0,"()"),(1,"IO ()"),(2,"String"),(3,"String -> IO ()")]
      _minfo_typeRefs `shouldBe` Map.fromList [ (5,[ModuleAst {_mast_span = Span {_span_lineStart = 5, _span_lineEnd = 5, _span_colStart = 0, _span_colEnd = 30}, _mast_specializedType = Just 1, _mast_generalizedType = Nothing, _mast_children = [ModuleAst {_mast_span = Span {_span_lineStart = 5, _span_lineEnd = 5, _span_colStart = 11, _span_colEnd = 19}, _mast_specializedType = Just 3, _mast_generalizedType = Nothing, _mast_children = []},ModuleAst {_mast_span = Span {_span_lineStart = 5, _span_lineEnd = 5, _span_colStart = 20, _span_colEnd = 30}, _mast_specializedType = Just 2, _mast_generalizedType = Nothing, _mast_children = []}]}])]

    it "project 1" $ do
      ModuleInfo{..} <- getProjectInfo "./izuna-builder/test/fixtures/hie/project1" "src/Lib.hs"
      _minfo_types `shouldBe` Map.fromList [(0,"()"),(1,"IO ()"),(2,"String"),(3,"String -> IO ()"),(4,"Int"),(5,"Int -> Int"),(6,"Int -> Int -> Int"),(7,"'LiftedRep"),(8,"*"),(9,"a"),(10,"Num a"),(11,"a -> a"),(12,"a -> a -> a"),(13,"Num a => a -> a -> a"),(14,"forall a. Num a => a -> a -> a"),(15,"[Int]"),(16,"[Int] -> Int"),(17,"Int -> [Int] -> Int"),(18,"(Int -> Int -> Int) -> Int -> [Int] -> Int"),(19,"* -> *"),(20,"t"),(21,"Foldable t"),(22,"b"),(23,"a"),(24,"a -> b"),(25,"b -> a -> b"),(26,"t a"),(27,"t a -> b"),(28,"b -> t a -> b"),(29,"(b -> a -> b) -> b -> t a -> b"),(30,"forall a. (b -> a -> b) -> b -> t a -> b"),(31,"forall b a. (b -> a -> b) -> b -> t a -> b"),(32,"forall b a. Foldable t => (b -> a -> b) -> b -> t a -> b"),(33,"forall (t :: * -> *) b a.\nFoldable t =>\n(b -> a -> b) -> b -> t a -> b"),(34,"Integer"),(35,"Integer -> String"),(36,"a"),(37,"Show a"),(38,"a -> String"),(39,"Show a => a -> String"),(40,"forall a. Show a => a -> String")]

getProjectInfo :: FilePath -> FilePath -> IO ModuleInfo
getProjectInfo filePath moduleName = do
  df <- RecoverType.getDynFlags
  modulesInfo <- App.buildProjectInfo filePath df
  modulesInfo &
    Map.lookup moduleName &
    Maybe.fromMaybe emptyModuleInfo &
    return
  where
    emptyModuleInfo :: ModuleInfo
    emptyModuleInfo =
      ModuleInfo { _minfo_types    = Map.empty
                 , _minfo_typeRefs = Map.empty
                 }
