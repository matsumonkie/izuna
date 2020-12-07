{- | This module re export common used external types in this project for convenience
-}

module Type where

import qualified Data.ByteString as ByteString
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified DynFlags        as Ghc
import qualified HieTypes        as Ghc
import           Numeric.Natural as Natural

type ByteString = ByteString.ByteString
type HieTypeFlat = Ghc.HieTypeFlat
type HieFile = Ghc.HieFile
type HieAST = Ghc.HieAST
type TypeIndex = Int
type PrintedType = String
type DynFlags = Ghc.DynFlags
type Set = S.Set
type NodeIdentifiers = Ghc.NodeIdentifiers String
type Text = T.Text
type Map = M.Map
type Nat = Natural.Natural
