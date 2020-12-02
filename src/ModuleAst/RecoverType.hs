module ModuleAst.RecoverType ( getDynFlags
                             , recoverTypes
                             ) where

import qualified Data.Array      as A

import           ModuleAst.Model
import           Type

import qualified CoreMonad       as Ghc
import           HieTypes        (HieArgs (..), HieType (..))
import           IfaceType
import           Name            (getOccFS)
import           Outputable      (showSDoc)
import           Var             (VarBndr (..))

--import           DynFlags     (defaultDynFlags)
import qualified GHC             as Ghc
import qualified GHC.Paths       as Ghc

-- * get dyn flags


getDynFlags :: IO DynFlags
getDynFlags = do
  Ghc.runGhc (Just Ghc.libdir) Ghc.getDynFlags


-- * recover full interface types

-- | Expand the flattened HIE AST into one where the types printed out and
-- ready for end-users to look at.
--
-- Using just primitives found in GHC's HIE utilities, we could write this as
-- follows:
--
-- > 'recoverFullIfaceTypes' dflags hieTypes hieAst
-- >     = 'fmap' (\ti -> 'showSDoc' df .
-- >                      'pprIfaceType' $
-- >                      'recoverFullType' ti hieTypes)
-- >       hieAst
--
-- However, this is very inefficient (both in time and space) because the
-- mutliple calls to 'recoverFullType' don't share intermediate results. This
-- function fixes that.
recoverTypes :: DynFlags -> RawModule TypeIndex a -> RawModule PrintedType a
recoverTypes df rawModule@RawModule{..} =
  rawModule { _rawModule_hieAst = fmap (printed A.!) _rawModule_hieAst }
    where

    -- Splitting this out into its own array is also important: we don't want
    -- to pretty print the same type many times
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
