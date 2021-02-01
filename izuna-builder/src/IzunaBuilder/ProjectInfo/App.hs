module IzunaBuilder.ProjectInfo.App ( saveProjectInfoHandler
                                    , buildProjectInfo
                                    ) where

-- * imports

-- ** aeson

import qualified Data.Aeson                           as Aeson

-- ** base

import qualified Control.Exception                    as Exception
import           Data.Function                        ((&))
import           Data.Functor                         ((<&>))
import qualified Data.List                            as List

-- ** tar

import qualified Codec.Archive.Tar                    as Tar

-- ** transformers

import qualified Control.Monad                        as Monad
--import qualified Control.Monad.Except                 as Except
import qualified Control.Monad.IO.Class               as IO

-- ** filepath

import           System.FilePath.Posix                ((</>))
import qualified System.FilePath.Posix                as FilePath

-- ** directory

import qualified System.Directory                     as Dir

-- ** servant

import           Servant.Multipart                    (FileData (..),
                                                       MultipartData (..), Tmp)
-- ** maybe

import qualified Data.Maybe                           as Maybe

-- ** containers

import qualified Data.Map                             as M

-- ** ghc

import qualified FastString                           as Ghc
import qualified GHC.Natural                          as Ghc
import qualified HieTypes                             as Ghc
import qualified SrcLoc                               as Ghc

-- ** async

import qualified Control.Concurrent.Async             as Async

--import           Debug.Pretty.Simple

-- ** local

import           IzunaBuilder.HieFile.App
import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.ProjectInfo.Model
import           IzunaBuilder.ProjectInfo.RecoverType
import           IzunaBuilder.ProjectInfo.Util
import           IzunaBuilder.Type

-- * handler

saveProjectInfoHandler
  :: (IO.MonadIO m)
  => NonEmptyString GhcVersion
  -> NonEmptyString Username
  -> NonEmptyString Repo
  -> NonEmptyString Commit
  -> [String]
  -> MultipartData Tmp
  -> m ()
saveProjectInfoHandler _ username repo commit projectRootAsList MultipartData{files} = do
  IO.liftIO $ do
    df <- getDynFlags
    createDirectory projectPath
    Monad.forM_ files $ extractHieTar hiePath
    _ <- Async.async $ do
      projectInfo <- buildProjectInfo hiePath df
      _ <- M.traverseWithKey (saveModuleInfo projectPath projectRoot) projectInfo
      Dir.removeDirectoryRecursive hiePath
    return ()
  where
    projectPath :: FilePath
    projectPath = getProjectPath username repo commit

    hiePath :: FilePath
    hiePath = getHiePath projectPath

    createDirectory :: FilePath -> IO ()
    createDirectory directory =
      Dir.createDirectoryIfMissing True directory

    extractHieTar :: FilePath -> FileData Tmp -> IO ()
    extractHieTar targetFolder FileData{..}=
      Tar.extract targetFolder fdPayload

    projectRoot :: FilePath
    projectRoot =
      FilePath.joinPath projectRootAsList


-- * build project info

buildProjectInfo
  :: FilePath
  -> DynFlags
  -> IO ModulesInfo
buildProjectInfo hieDirectory df = do
  hieFiles <- getHieFiles
  let filePathToRawModule = getFilePathToRawModule hieFiles & M.map removeUselessNodes
  return $ M.map (\rawModule ->
                    ModuleInfo { _minfo_types = recoverTypes df rawModule
                               , _minfo_typeRefs = buildModuleInfo rawModule
                               }
                 ) filePathToRawModule
  where
    getHieFiles :: IO [HieFile]
    getHieFiles = do
      hieFiles <- parseHieFiles [ hieDirectory ]
      hieFiles & filter (not . generatedFile) & return
        where
          generatedFile :: HieFile -> Bool
          generatedFile Ghc.HieFile {..} =
            ".stack-work" `List.isPrefixOf` hie_hs_file

    getFilePathToRawModule :: [HieFile] -> Map FilePath (RawModule TypeIndex ByteString)
    getFilePathToRawModule hieFiles = do
      convertHieFilesToMap hieFiles
        where
          convertHieFilesToMap :: [HieFile] -> M.Map FilePath (RawModule TypeIndex ByteString)
          convertHieFilesToMap hieFiles =
            hieFiles <&> convertHieToRawModule & M.fromList

    buildModuleInfo :: RawModule TypeIndex ByteString -> Map Nat [ModuleAst]
    buildModuleInfo rawModule =
      rawModule & convertRawModuleToModuleAst & groupByLine

-- * convert raw module to raw lines

-- | instead of handling data as a whole, we split it by line of code
-- doing so will help further down the pipe when we need to generate DOM (that can't handle multiline yet)
convertRawModuleToModuleAst :: RawModule TypeIndex ByteString -> ModuleAst
convertRawModuleToModuleAst RawModule{..} =
  hieAstToModuleAst _rawModule_hieAst
  where
    hieAstToModuleAst :: HieAST TypeIndex -> ModuleAst
    hieAstToModuleAst Ghc.Node{..} =
      ModuleAst { _mast_span =
                  -- line and column starts at 1 in hie ast
                  Span { _span_lineStart = Ghc.intToNatural $ Ghc.srcSpanStartLine nodeSpan - 1
                       , _span_lineEnd   = Ghc.intToNatural $ Ghc.srcSpanEndLine nodeSpan - 1
                       , _span_colStart  = Ghc.intToNatural $ colPos $ Ghc.srcSpanStartCol nodeSpan
                       , _span_colEnd    = Ghc.intToNatural $ colPos $ Ghc.srcSpanEndCol nodeSpan
                       }
                , _mast_specializedType = nodeInfo & Ghc.nodeType & specializedAndGeneralizedType & fst
                , _mast_generalizedType = nodeInfo & Ghc.nodeType & specializedAndGeneralizedType & snd
                , _mast_children = nodeChildren <&> hieAstToModuleAst
                }
      where
        {- | I don't understand this... Sometimes, hie returns either 0 or a negative value the column position for. todo: investigate -}
        colPos :: Int -> Int
        colPos = \case
          0 -> 0
          positiveNumber -> positiveNumber - 1

        specializedAndGeneralizedType :: [TypeIndex] -> (Maybe TypeIndex, Maybe TypeIndex)
        specializedAndGeneralizedType = \case
          [s, g] -> (Just s, Just g)
          [s]    -> (Just s, Nothing)
          _      -> (Nothing, Nothing)

-- * save modules info

saveModuleInfo
  :: FilePath
  -> FilePath
  -> FilePath
  -> ModuleInfo
  -> IO ()
saveModuleInfo projectPath projectRoot filePath projectInfo = do
  let (subDir, filename) = FilePath.splitFileName filePath
  Dir.createDirectoryIfMissing True (jsonPath </> subDir)
  Exception.try (Aeson.encodeFile (jsonPath </> subDir </> filename) projectInfo) >>= \case
    Left (exception :: Exception.IOException) -> do
      putStrLn $ "Error while saving file:" <> filePath <> " in: " <> projectPath <> " - " <> show exception
      return ()
    Right _ -> return ()
  where
    jsonPath :: FilePath
    jsonPath =
      getJsonPath projectPath projectRoot

-- * convert hie to raw module


convertHieToRawModule :: HieFile -> (FilePath, RawModule TypeIndex ByteString)
convertHieToRawModule hie@Ghc.HieFile {..} =
  ( hie_hs_file
  , RawModule { _rawModule_hieTypes = hie_types
              , _rawModule_hieAst = hieAstsToAst hie
              , _rawModule_fileContent = hie_hs_src
              }
  )
  where
    hieAstsToAst :: HieFile -> HieAST TypeIndex
    hieAstsToAst Ghc.HieFile { hie_asts = Ghc.HieASTs asts
                             , hie_hs_file
                             } =
      Maybe.fromMaybe (emptyHieAst fileFs) mast
      where
        fileFs :: Ghc.FastString
        fileFs = Ghc.mkFastString hie_hs_file

        mast :: Maybe (HieAST TypeIndex)
        mast =
          case M.size asts == 1 of
            True  -> M.lookupMin asts <&> snd
            False -> M.lookup fileFs asts

        emptyHieAst :: Ghc.FastString -> HieAST TypeIndex
        emptyHieAst fileFs = Ghc.Node
          { nodeInfo = emptyNodeInfo
          , nodeSpan = Ghc.realSrcLocSpan (Ghc.mkRealSrcLoc fileFs 1 0)
          , nodeChildren = []
          }

        emptyNodeInfo :: Ghc.NodeInfo TypeIndex
        emptyNodeInfo = Ghc.NodeInfo
          { nodeAnnotations = mempty
          , nodeType = []
          , nodeIdentifiers = mempty
          }

-- * remove useless nodes

-- | given a tree, if a node of this tree doesn't contain any informations and doesn't have any
-- children, we get rid of it
removeUselessNodes :: RawModule a b -> RawModule a b
removeUselessNodes rawModule@RawModule{ _rawModule_hieAst = ast  } =
  rawModule { _rawModule_hieAst = ast { Ghc.nodeChildren = foldr go [] $ Ghc.nodeChildren ast }}
  where
    go :: HieAST a -> [HieAST a] -> [HieAST a]
    go hieAst@Ghc.Node{..} acc =
      case (nodeChildren, hasSpecializedType $ nodeInfo & Ghc.nodeType) of
        ([], False) -> acc
        (_, False) -> foldr go [] nodeChildren ++ acc
        _ -> hieAst { Ghc.nodeChildren = foldr go [] nodeChildren } : acc

    hasSpecializedType :: [a] -> Bool
    hasSpecializedType = not . List.null


-- * group by line


groupByLine :: ModuleAst -> Map Nat [ModuleAst]
groupByLine moduleAst2 =
    List.foldl' go M.empty [moduleAst2]
  where
    go :: Map Nat [ModuleAst] -> ModuleAst -> Map Nat [ModuleAst]
    go acc moduleAst@ModuleAst{..} =
      case indexOneLineSpan _mast_span of
        Nothing    -> List.foldl' go acc _mast_children
        Just index -> M.insertWith (flip (++)) index [moduleAst] acc

    indexOneLineSpan :: Span -> Maybe Nat
    indexOneLineSpan span@Span{..} =
      case isOneLine span of
        False -> Nothing
        True  -> Just _span_lineStart


test :: Int
test =
  1 + 1
