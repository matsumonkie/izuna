module ModuleAst.Model ( RawModule(..)
                       , ModuleAst(..)
                       , ModulesAst
                       , ModuleInfo(..)
                       , LineAst(..)
                       , Span(..)
                       , isOneLine
                       ) where


-- * imports


-- ** base

--import           Data.Function    ((&))
--import           Data.Functor     ((<&>))

-- ** aeson

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
--import qualified Data.Aeson.Types as Aeson

-- ** array

import           Data.Array (Array)

{-
-- ** ghc

import qualified BasicTypes as Ghc
import qualified FastString as Ghc
import qualified GhcPlugins as Ghc
import qualified HieTypes   as Ghc
import qualified IfaceType  as Ghc
-}

-- ** local

import           Type


-- * model


data RawModule ast line = RawModule
    { _rawModule_hieTypes    :: Array TypeIndex HieTypeFlat
    , _rawModule_hieAst      :: HieAST ast
    , _rawModule_fileContent :: line
    }

data LineAst = LineAst
    { _lineAst_line :: Text
    , _lineAst_asts :: [ModuleAst]
    }

data Span = Span
    { _span_lineStart :: Nat
    , _span_lineEnd   :: Nat
    , _span_colStart  :: Nat
    , _span_colEnd    :: Nat
    }
    deriving (Show, Eq)

isOneLine :: Span -> Bool
isOneLine Span{..} =
  _span_lineStart == _span_lineEnd

data ModuleInfo = ModuleInfo
    { _minfo_asts        :: [ModuleAst]
    , _minfo_fileContent :: [Text]
    }
    deriving Show

type ModulesAst = Map FilePath ModuleInfo

data ModuleAst = ModuleAst
    { _mast_span            :: Span
    , _mast_specializedType :: Maybe String
    , _mast_generalizedType :: Maybe String
    , _mast_children        :: [ModuleAst]
    }
    deriving (Show, Eq)

instance Ord ModuleAst where
  compare moduleAst1 moduleAst2 =
    compare (showCoord moduleAst1) (showCoord moduleAst2)
    where
      showCoord :: ModuleAst -> String
      showCoord ModuleAst{ _mast_span = Span{..} } =
        show _span_lineStart <> ":" <> show _span_lineStart <> ":" <> show _span_colStart <> ":" <> show _span_colEnd


-- * json

instance Aeson.ToJSON ModuleInfo where
  toJSON ModuleInfo{..} =
    Aeson.object [ "asts" .= _minfo_asts
                 , "fileContent" .= _minfo_fileContent
                 ]

instance Aeson.ToJSON ModuleAst where
  toJSON ModuleAst {..} =
    Aeson.object [ "span" .= _mast_span
                 , "specializedType" .= _mast_specializedType
                 , "generalizedType" .= _mast_generalizedType
                 , "children" .= _mast_children
                 ]

instance Aeson.ToJSON Span where
  toJSON Span{..} =
    Aeson.toJSON $ show _span_lineStart <> ":" <> show _span_lineEnd <> " " <> show _span_colStart <> ":" <> show _span_colEnd

{-
instance Aeson.ToJSON (Ghc.IdentifierDetails PrintedType) where
  toJSON Ghc.IdentifierDetails{..} =
    Aeson.object [ "identType" .= identType
                 , "identInfo" .= (identInfo & S.elems <&> show <&> T.pack)
                 ]
-}

-- * other

{-
instance Aeson.ToJSON (Ghc.HieType Ghc.TypeIndex) where
  toJSON = \case
    Ghc.HTyVarTy name -> Aeson.toJSON $ "HTyVarTy: " <> Ghc.nameStableString name
    Ghc.HAppTy typeIndex args -> Aeson.toJSON $ "HAppTy: " <> show typeIndex
    Ghc.HTyConApp Ghc.IfaceTyCon{..} args ->
      Aeson.object [ "hieType" .= ("HTyConApp" :: String)
                   , "ifaceTyConName" .= (Ghc.nameStableString ifaceTyConName)
                   , "ifaceTyConInfo.promoted" .= (ifaceTyConInfo & Ghc.ifaceTyConIsPromoted & Ghc.isPromoted)
                   , "ifaceTyConInfo.sort" .=
                     ( ifaceTyConInfo & Ghc.ifaceTyConSort & \case
                         Ghc.IfaceNormalTyCon -> ("IfaceNormalTyCon" :: String)
                         Ghc.IfaceTupleTyCon _ _ -> ("IfaceTupleTyCon" :: String)
                         Ghc.IfaceSumTyCon _ -> ("IfaceSumTyCon" :: String)
                         Ghc.IfaceEqualityTyCon -> ("IfaceEqualityTyCon" :: String)
                     )
                   ]
    Ghc.HForAllTy (name, arg) a -> "HForAllTy" -- add serialization for all params below
    Ghc.HFunTy a b -> Aeson.String $ T.pack $ "HFunTy:" <> show a <> ":" <> show b
    Ghc.HQualTy _ _ -> "HQualTy"
    Ghc.HLitTy ifaceTyLit -> "HLitTy"
    Ghc.HCastTy a -> "HCastTy"
    Ghc.HCoercionTy -> "HCoercionTy"

instance Aeson.ToJSON (Ghc.HieAST Ghc.TypeIndex) where
  toJSON Ghc.Node { nodeInfo, nodeSpan, nodeChildren } =
    Aeson.object [ "nodeInfo.nodeAnnotations" .= (nodeInfo & Ghc.nodeAnnotations)
                 , "nodeInfo.nodeType" .= (nodeInfo & Ghc.nodeType)
                 , "nodeInfo.nodeIdentifiers" .= (nodeInfo & Ghc.nodeIdentifiers)
                 , "nodeSpan.file" .= (nodeSpan & Ghc.srcSpanFile & Ghc.unpackFS)
                 , "nodeSpan.loc" .= ( (show $ Ghc.srcSpanStartLine nodeSpan) <>
                                       ":" <>
                                       (show $ Ghc.srcSpanEndLine nodeSpan) <>
                                       " " <>
                                       (show $ Ghc.srcSpanStartCol nodeSpan) <>
                                       ":" <>
                                       (show $ Ghc.srcSpanEndCol nodeSpan)
                                     )
                 , "nodeChildren" .= Aeson.toJSON nodeChildren
                 ]

instance Aeson.ToJSON Ghc.FastString where
  toJSON =
    Aeson.String . T.pack . Ghc.unpackFS

instance Aeson.ToJSON (Ghc.IdentifierDetails Ghc.TypeIndex) where
  toJSON Ghc.IdentifierDetails{..} =
    Aeson.object [ "identType" .= identType
                 , "identInfo" .= (identInfo & S.elems <&> show <&> T.pack)
                 ]

instance Aeson.ToJSONKey (Either Ghc.ModuleName Ghc.Name) where
  toJSONKey =
    Aeson.toJSONKeyText str
    where
      str :: Either Ghc.ModuleName Ghc.Name -> T.Text
      str either =
        T.pack $ case either of
          Left moduleName -> Ghc.moduleNameString moduleName
          Right name      -> Ghc.nameStableString name

instance Aeson.ToJSON Ghc.ModuleName where
  toJSON moduleName =
    Aeson.String $ T.pack $ Ghc.moduleNameString moduleName

instance Aeson.ToJSON Ghc.Name where
  toJSON name =
    Aeson.String $ T.pack $ Ghc.nameStableString name
-}
