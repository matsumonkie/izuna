{-
{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-}

module Json where

{-
-- * imports


-- ** aeson

import           Data.Aeson       ((.=))
import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson

-- ** base

import           Data.Array
import           Data.Function    ((&))
import           Data.Functor     ((<&>))
import qualified Data.Set         as Set
import           Prelude          hiding (span)

-- ** text

import qualified Data.Text        as T

-- ** ghc

import           BasicTypes
import           FastString
import           GHC
import           HieTypes         (BindType (RegularBind), ContextInfo (ClassTyDecl, Decl, PatternBind, TyDecl, Use, ValBind),
                                   DeclType (ClassDec, ConDec, DataDec),
                                   HieAST (Node, nodeChildren, nodeInfo, nodeSpan),
                                   HieASTs (..), HieArgs (..), HieFile,
                                   HieFile (HieFile, hie_asts, hie_exports, hie_hs_file, hie_hs_src, hie_module, hie_types),
                                   HieType (..), HieTypeFlat,
                                   IdentifierDetails (IdentifierDetails, identInfo, identType),
                                   NodeInfo (NodeInfo, nodeAnnotations, nodeIdentifiers, nodeType),
                                   Scope (ModuleScope), TypeIndex, hieVersion)
import           IfaceType
import           Name             (nameStableString)


-- * aeson hiefile type index

instance Aeson.ToJSON HieFile where
  toJSON HieFile{ hie_hs_file
                , hie_types
                , hie_exports
                , hie_module
                , hie_asts = HieASTs asts
                } =
    Aeson.object [ "hie_hs_file" .= hie_hs_file
                 , "moduleUnitId" .= show (hie_module & moduleUnitId)
                 , "moduleName" .= moduleNameString (hie_module & moduleName)
                 , "hie_types"   .= elems hie_types
                 , "hie_asts"    .= asts
                 ]

instance Aeson.ToJSON (IdentifierDetails TypeIndex) where
  toJSON IdentifierDetails{..} =
    Aeson.object [ "identType" .= identType
                 , "identInfo" .= (identInfo & Set.elems <&> show <&> T.pack)
                 ]

instance Aeson.ToJSON (HieAST TypeIndex) where
  toJSON Node { nodeInfo, nodeSpan, nodeChildren } =
    Aeson.object [ "nodeInfo.nodeAnnotations" .= (nodeInfo & nodeAnnotations)
                 , "nodeInfo.nodeType" .= (nodeInfo & nodeType)
                 , "nodeInfo.nodeIdentifiers" .= (nodeInfo & nodeIdentifiers)
                 , "nodeSpan.file" .= (nodeSpan & srcSpanFile & unpackFS)
                 , "nodeSpan.loc" .= ( (show $ srcSpanStartLine nodeSpan) <>
                                       ":" <>
                                       (show $ srcSpanEndLine nodeSpan) <>
                                       " " <>
                                       (show $ srcSpanStartCol nodeSpan) <>
                                       ":" <>
                                       (show $ srcSpanEndCol nodeSpan)
                                     )
                 , "nodeChildren" .= Aeson.toJSON nodeChildren
                 ]

instance Aeson.ToJSON (HieType TypeIndex) where
  toJSON = \case
    HTyVarTy name -> Aeson.toJSON $ "HTyVarTy: " <> nameStableString name
    HAppTy typeIndex args -> Aeson.toJSON $ "HAppTy: " <> show typeIndex
    HTyConApp IfaceTyCon{..} args ->
      Aeson.object [ "hieType" .= ("HTyConApp" :: String)
                   , "ifaceTyConName" .= (nameStableString ifaceTyConName)
                   , "ifaceTyConInfo.promoted" .= (ifaceTyConInfo & ifaceTyConIsPromoted & isPromoted)
                   , "ifaceTyConInfo.sort" .=
                     ( ifaceTyConInfo & ifaceTyConSort & \case
                         IfaceNormalTyCon -> ("IfaceNormalTyCon" :: String)
                         IfaceTupleTyCon _ _ -> ("IfaceTupleTyCon" :: String)
                         IfaceSumTyCon _ -> ("IfaceSumTyCon" :: String)
                         IfaceEqualityTyCon -> ("IfaceEqualityTyCon" :: String)
                     )
                   ]
    HForAllTy (name, arg) a -> "HForAllTy" -- add serialization for all params below
    HFunTy a b -> Aeson.String $ T.pack $ "HFunTy:" <> show a <> ":" <> show b
    HQualTy _ _ -> "HQualTy"
    HLitTy ifaceTyLit -> "HLitTy"
    HCastTy a -> "HCastTy"
    HCoercionTy -> "HCoercionTy"

instance Aeson.ToJSONKey FastString where
  toJSONKey =
    Aeson.toJSONKeyText (T.pack . unpackFS)

instance Aeson.ToJSON FastString where
  toJSON =
    Aeson.String . T.pack . unpackFS

instance Aeson.ToJSON ModuleName where
  toJSON moduleName =
    Aeson.String $ T.pack $ moduleNameString moduleName

instance Aeson.ToJSON Name where
  toJSON name =
    Aeson.String $ T.pack $ nameStableString name

instance Aeson.ToJSONKey (Either ModuleName Name) where
  toJSONKey =
    Aeson.toJSONKeyText str
    where
      str :: Either ModuleName Name -> T.Text
      str either =
        T.pack $ case either of
          Left moduleName -> moduleNameString moduleName
          Right name      -> nameStableString name


-- * aeson hiefile printed type

type PrintedType = String

instance Aeson.ToJSON (IdentifierDetails PrintedType) where
  toJSON IdentifierDetails{..} =
    Aeson.object [ "identType" .= identType
                 , "identInfo" .= (identInfo & Set.elems <&> show <&> T.pack)
                 ]

instance Aeson.ToJSON (HieAST PrintedType) where
  toJSON Node { nodeInfo, nodeSpan, nodeChildren } =
    Aeson.object [ "nodeInfo.nodeAnnotations" .= (nodeInfo & nodeAnnotations)
                 , "nodeInfo.nodeType" .= (nodeInfo & nodeType)
                 , "nodeInfo.nodeIdentifiers" .= (nodeInfo & nodeIdentifiers)
                 , "nodeSpan.file" .= (nodeSpan & srcSpanFile & unpackFS)
                 , "nodeSpan.loc" .= ( (show $ srcSpanStartLine nodeSpan) <>
                                       ":" <>
                                       (show $ srcSpanEndLine nodeSpan) <>
                                       " " <>
                                       (show $ srcSpanStartCol nodeSpan) <>
                                       ":" <>
                                       (show $ srcSpanEndCol nodeSpan)
                                     )
                 , "nodeChildren" .= Aeson.toJSON nodeChildren
                 ]

instance Aeson.ToJSON (HieType PrintedType) where
  toJSON = \case
    HTyVarTy name -> Aeson.toJSON $ "HTyVarTy: " <> nameStableString name
    HAppTy typeIndex args -> Aeson.toJSON $ "HAppTy: " <> show typeIndex
    HTyConApp IfaceTyCon{..} args ->
      Aeson.object [ "hieType" .= ("HTyConApp" :: String)
                   , "ifaceTyConName" .= (nameStableString ifaceTyConName)
                   , "ifaceTyConInfo.promoted" .= (ifaceTyConInfo & ifaceTyConIsPromoted & isPromoted)
                   , "ifaceTyConInfo.sort" .=
                     ( ifaceTyConInfo & ifaceTyConSort & \case
                         IfaceNormalTyCon -> ("IfaceNormalTyCon" :: String)
                         IfaceTupleTyCon _ _ -> ("IfaceTupleTyCon" :: String)
                         IfaceSumTyCon _ -> ("IfaceSumTyCon" :: String)
                         IfaceEqualityTyCon -> ("IfaceEqualityTyCon" :: String)
                     )
                   ]
    HForAllTy (name, arg) a -> "HForAllTy" -- add serialization for all params below
    HFunTy a b -> Aeson.String $ T.pack $ "HFunTy:" <> show a <> ":" <> show b
    HQualTy _ _ -> "HQualTy"
    HLitTy ifaceTyLit -> "HLitTy"
    HCastTy a -> "HCastTy"
    HCoercionTy -> "HCoercionTy"
-}
