module IzunaBuilder.ProjectInfo.RecoverType ( getDynFlags
                                            , recoverTypes
                                            ) where

import qualified CoreMonad                      as Ghc
import qualified Data.Array                     as A
import           Data.Function                  ((&))
import qualified Data.Map                       as Map
import qualified GHC                            as Ghc
import qualified GHC.Paths                      as Ghc
import           HieTypes                       (HieArgs (..), HieType (..))
import           IfaceType
import           Name                           (getOccFS)
import           Outputable                     (showSDoc)
import           Var                            (VarBndr (..))

import           IzunaBuilder.ProjectInfo.Model
import           IzunaBuilder.Type

-- * get dyn flags


getDynFlags :: IO DynFlags
getDynFlags = do
  Ghc.runGhc (Just Ghc.libdir) Ghc.getDynFlags

-- * recover types

recoverTypes :: DynFlags -> RawModule TypeIndex a -> Map TypeIndex PrintedType
recoverTypes df RawModule{..} =
  printed & A.assocs & Map.fromList
  where
    printed :: A.Array TypeIndex PrintedType
    printed = fmap (showSDoc df . pprIfaceType) unflattened

    -- The recursion in 'unflattened' is crucial - it's what gives us sharing
    -- between the IfaceType's produced
    unflattened :: A.Array TypeIndex IfaceType
    unflattened = fmap (go . fmap (unflattened A.!)) _rawModule_hieTypes

    -- Unfold an 'HieType' whose subterms have already been unfolded
    go :: HieType IfaceType -> IfaceType
    go (HTyVarTy n) = IfaceTyVar (getOccFS n)
    go (HAppTy a b) = IfaceAppTy a (hieToIfaceArgs b)
    go (HLitTy l) = IfaceLitTy l
    go (HForAllTy ((n,k),af) t) = let b = (getOccFS n, k)
                                  in IfaceForAllTy (Bndr (IfaceTvBndr b) af) t
    go (HFunTy a b) = IfaceFunTy VisArg a b
    go (HQualTy con b) = IfaceFunTy InvisArg con b
    go (HCastTy a) = a
    go HCoercionTy = IfaceTyVar "<coercion type>"
    go (HTyConApp a xs) = IfaceTyConApp a (hieToIfaceArgs xs)

    -- This isn't fully faithful - we can't produce the 'Inferred' case
    hieToIfaceArgs :: HieArgs IfaceType -> IfaceAppArgs
    hieToIfaceArgs (HieArgs args) = go' args
      where
        go' []             = IA_Nil
        go' ((True ,x):xs) = IA_Arg x Required $ go' xs
        go' ((False,x):xs) = IA_Arg x Specified $ go' xs
