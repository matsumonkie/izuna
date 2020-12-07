module ProjectInfo.App ( getProjectInfo
                     ) where

-- * imports

-- ** base

import           Data.Function           ((&))
import           Data.Functor            ((<&>))
import qualified Data.List               as List

-- ** maybe

import qualified Data.Maybe              as Maybe

-- ** containers

import qualified Data.Map                as M

-- ** ghc

import qualified FastString              as Ghc
import qualified GHC.Natural             as Ghc
import qualified HieTypes                as Ghc
import qualified SrcLoc                  as Ghc

-- ** local

import           BuilderConfig.App
import           HieFile.App
import           ProjectInfo.Model
import           ProjectInfo.RecoverType
import           Type

-- ** text

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

--import           Debug.Pretty.Simple

-- * handler


getProjectInfo :: BuilderConfig -> IO ProjectInfo
getProjectInfo BuilderConfig{..} = do
  dynFlags <- getDynFlags
  hieFiles <- parseHieFiles [ _builderConfig_hieDirectory ]
  let modulesInfo =
        hieFiles &
        filter (not . generatedFile) &
        convertHieFilesToMap &
        M.map (\rawModule -> rawModule & convertContentToText & buildAst dynFlags & generateDom)
  return $ ProjectInfo { _projectInfo_modulesInfo = modulesInfo
                       , _projectInfo_user = _builderConfig_user
                       , _projectInfo_repo = _builderConfig_repo
                       , _projectInfo_package = _builderConfig_package
                       , _projectInfo_commit = _builderConfig_commit
                       , _projectInfo_publicRepo = _builderConfig_publicRepo
                       }
  where
    generatedFile :: HieFile -> Bool
    generatedFile Ghc.HieFile {..} =
      ".stack-work" `List.isPrefixOf` hie_hs_file

    convertHieFilesToMap :: [HieFile] -> M.Map FilePath (RawModule TypeIndex ByteString)
    convertHieFilesToMap hieFiles =
      hieFiles <&> convertHieToRawModule & M.fromList

    buildAst :: DynFlags -> RawModule TypeIndex [Text] -> [LineAst]
    buildAst dynFlags rawModule =
      rawModule &
      recoverTypes dynFlags &
      removeUselessNodes &
      convertRawModuleToLineAst <&>
      fillInterval


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
removeUselessNodes :: RawModule PrintedType a -> RawModule PrintedType a
removeUselessNodes rawModule@RawModule{ _rawModule_hieAst = ast  } =
  rawModule { _rawModule_hieAst = ast { Ghc.nodeChildren = foldr go [] $ Ghc.nodeChildren ast }}
  where
    go :: HieAST PrintedType -> [HieAST PrintedType] -> [HieAST PrintedType]
    go hieAst@Ghc.Node{..} acc =
      case (nodeChildren, hasSpecializedType $ nodeInfo & Ghc.nodeType) of
        ([], False) -> acc
        (_, False) -> foldr go [] nodeChildren ++ acc
        _ -> hieAst { Ghc.nodeChildren = foldr go [] nodeChildren } : acc

    hasSpecializedType :: [PrintedType] -> Bool
    hasSpecializedType = not . List.null


-- * convert content to text

convertContentToText :: RawModule a ByteString -> RawModule a [Text]
convertContentToText rawModule@RawModule{..} =
  rawModule { _rawModule_fileContent = _rawModule_fileContent & T.decodeUtf8 & T.lines
            }


-- * convert raw module to raw lines

-- | instead of handling data as a whole, we split by line of code
-- doing so will help further down the pipe when we need to generate DOM (that can't handle multiline yet)
convertRawModuleToLineAst :: RawModule PrintedType [Text] -> [ (Nat, LineAst) ]
convertRawModuleToLineAst RawModule{..} =
    linesWithIndex <&> buildDom groupedByLine
  where
    linesWithIndex :: [(Nat, Text)]
    linesWithIndex = List.zip [0..] _rawModule_fileContent

    groupedByLine :: Map Nat [ModuleAst]
    groupedByLine =
      groupByLineIndex [ hieAstToModuleAst _rawModule_hieAst ]

    groupByLineIndex :: [ModuleAst] -> Map Nat [ModuleAst]
    groupByLineIndex modulesAst =
      List.foldl' go M.empty modulesAst
      where
        go :: Map Nat [ModuleAst] -> ModuleAst -> Map Nat [ModuleAst]
        go acc moduleAst@ModuleAst{..} =
          case indexOneLineSpan _mast_span of
            Nothing    -> List.foldl' go acc _mast_children
            Just index -> M.insertWith (flip (++)) index [moduleAst] acc

    hieAstToModuleAst :: HieAST PrintedType -> ModuleAst
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

        specializedAndGeneralizedType :: [PrintedType] -> (Maybe String, Maybe String)
        specializedAndGeneralizedType = \case
          [s, g] -> (Just s, Just g)
          [s]     -> (Just s, Nothing)
          _          -> (Nothing, Nothing)

    indexOneLineSpan :: Span -> Maybe Nat
    indexOneLineSpan span@Span{..} =
      case isOneLine span of
        False -> Nothing
        True  -> Just _span_lineStart

    buildDom :: Map Nat [ModuleAst] -> (Nat, Text) -> (Nat, LineAst)
    buildDom indexToAst (index, line) =
      case M.lookup index indexToAst of
        Nothing  -> (index, LineAst line [])
        Just ast -> (index, LineAst line ast)

-- * fill interval

-- | this function takes a list of Ast for a given line and make sure
-- it covers the whole line by filling the ast's gaps
fillInterval :: (Nat, LineAst) -> LineAst
fillInterval (index, LineAst line asts) =
  let
    max = Ghc.intToNatural $  T.length line
    (newMax, filledIntervals) = List.foldr go (max, []) asts
  in
    LineAst line $ fillBeginning True index 0 newMax filledIntervals
  where
    go :: ModuleAst -> (Nat, [ModuleAst]) -> (Nat, [ModuleAst])
    go moduleAst@ModuleAst{ _mast_children
                          , _mast_span = Span{ _span_colStart, _span_colEnd }
                          } (max, asts) =
      let
        (newMax, newChildrenIntervals) = List.foldr go (_span_colEnd, []) _mast_children
        newChildren = fillBeginning False index _span_colStart newMax newChildrenIntervals
        updatedModule = moduleAst { _mast_children = newChildren }
      in
      case _span_colEnd < max of
        True ->
          ( _span_colStart
          , updatedModule
            : ModuleAst { _mast_span =
                          Span { _span_lineStart = index
                               , _span_lineEnd   = index
                               , _span_colStart  = _span_colEnd
                               , _span_colEnd    = max
                               }
                        , _mast_specializedType = Nothing
                        , _mast_generalizedType = Nothing
                        , _mast_children = []
                        }
            : asts
         )

        False ->
            (_span_colStart, updatedModule : asts)

    fillBeginning :: Bool -> Nat -> Nat -> Nat -> [ModuleAst] -> [ModuleAst]
    fillBeginning isTop index min max intervals =
      case (isTop, intervals) of
        (False, []) -> []
        _ -> case min /= max of
          False -> intervals
          True ->
            ModuleAst { _mast_span =
                        Span { _span_lineStart = index
                             , _span_lineEnd   = index
                             , _span_colStart  = min
                             , _span_colEnd    = max
                             }
                      , _mast_specializedType = Nothing
                      , _mast_generalizedType = Nothing
                      , _mast_children = []
                      } : intervals

-- * generate dom


generateDom :: [LineAst] -> ModuleInfo
generateDom linesAsts =
  ModuleInfo { _minfo_asts = linesAsts >>= _lineAst_asts
             , _minfo_fileContent = linesAsts <&> renderLine
             }
  where
    renderLine :: LineAst -> Text
    renderLine LineAst{..} =
      List.foldr (go _lineAst_line) "" _lineAst_asts

    go :: Text -> ModuleAst -> Text -> Text
    go line ModuleAst{..} acc =
      case _mast_children of
        [] -> render (fetchTextInSpan _mast_span) <> acc
        _  -> List.foldr (go line) acc _mast_children
      where
        render :: Text -> Text
        render text =
          case _mast_specializedType of
            Nothing -> text
            Just t  -> "<span data-specialized-type='" <> T.pack t <> "'>" <> text <> "</span>"

        fetchTextInSpan :: Span -> Text
        fetchTextInSpan Span{..} =
          line
            & T.drop (Ghc.naturalToInt _span_colStart)
            & T.take (Ghc.naturalToInt (_span_colEnd - _span_colStart))
