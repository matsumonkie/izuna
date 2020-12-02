module Type where

-- * base

import           Numeric.Natural as Natural

-- * ghc

import qualified DynFlags        as Ghc
import qualified HieTypes        as Ghc

-- * text

import qualified Data.Text       as T

-- * containers

import qualified Data.Map        as M
import qualified Data.Set        as S

-- * bytestring

import qualified Data.ByteString as ByteString

-- * type

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
