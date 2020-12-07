{-# LANGUAGE DeriveGeneric #-}

module IzunaBuilder.ProjectInfo.Model ( RawModule(..)
                                      , ModuleAst(..)
                                      , ModulesInfo
                                      , ModuleInfo(..)
                                      , ProjectInfo(..)
                                      , LineAst(..)
                                      , Span(..)
                                      , isOneLine
                                      ) where


-- * imports


-- ** aeson

import qualified Data.Aeson                  as Aeson

-- ** array

import           Data.Array                  (Array)


-- ** ghc

import           GHC.Generics
{-
import qualified BasicTypes as Ghc
import qualified FastString as Ghc
import qualified GhcPlugins as Ghc
import qualified HieTypes   as Ghc
import qualified IfaceType  as Ghc
-}

-- ** local

import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.Type

-- * model

-- ** final

data ProjectInfo = ProjectInfo
    { _projectInfo_modulesInfo :: ModulesInfo
    , _projectInfo_user        :: NonEmptyString
    , _projectInfo_repo        :: NonEmptyString
    , _projectInfo_package     :: NonEmptyString
    , _projectInfo_commit      :: NonEmptyString
    , _projectInfo_publicRepo  :: Bool
    }
    deriving (Generic)

type ModulesInfo = Map FilePath ModuleInfo

data ModuleInfo = ModuleInfo
    { _minfo_asts        :: [ModuleAst]
    , _minfo_fileContent :: [Text]
    }
    deriving (Show, Generic)

data ModuleAst = ModuleAst
    { _mast_span            :: Span
    , _mast_specializedType :: Maybe String
    , _mast_generalizedType :: Maybe String
    , _mast_children        :: [ModuleAst]
    }
    deriving (Show, Eq, Generic)

data Span = Span
    { _span_lineStart :: Nat
    , _span_lineEnd   :: Nat
    , _span_colStart  :: Nat
    , _span_colEnd    :: Nat
    }
    deriving (Show, Eq, Generic)

isOneLine :: Span -> Bool
isOneLine Span{..} =
  _span_lineStart == _span_lineEnd

-- ** initial


data RawModule ast line = RawModule
    { _rawModule_hieTypes    :: Array TypeIndex HieTypeFlat
    , _rawModule_hieAst      :: HieAST ast
    , _rawModule_fileContent :: line
    }

-- ** intermediate

data LineAst = LineAst
    { _lineAst_line :: Text
    , _lineAst_asts :: [ModuleAst]
    }
    deriving Show


-- * json

instance Aeson.ToJSON ProjectInfo where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("_projectInfo_" :: String) }

instance Aeson.ToJSON ModuleInfo where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("_minfo_" :: String) }

instance Aeson.FromJSON ModuleInfo where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("_minfo_" :: String) }

instance Aeson.ToJSON ModuleAst where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("_mast_" :: String) }

instance Aeson.FromJSON ModuleAst where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("_mast_" :: String) }

instance Aeson.ToJSON Span where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("_span_" :: String) }

instance Aeson.FromJSON Span where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("_span_" :: String) }


-- * other

{-
instance Aeson.ToJSON ModuleInfo where
  toJSON ModuleInfo{..} =
    Aeson.object [ "asts" .= _minfo_asts
                 , "fileContent" .= _minfo_fileContent
                 ]

instance Aeson.FromJSON ModuleInfo where
  fromJSON ModuleInfo{..} =
    Aeson.object [ "asts" .= _minfo_asts
                 , "fileContent" .= _minfo_fileContent
                 ]
-}

{-
instance Aeson.ToJSON (Ghc.IdentifierDetails PrintedType) where
  toJSON Ghc.IdentifierDetails{..} =
    Aeson.object [ "identType" .= identType
                 , "identInfo" .= (identInfo & S.elems <&> show <&> T.pack)
                 ]

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
