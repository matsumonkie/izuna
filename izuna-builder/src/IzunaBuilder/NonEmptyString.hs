module IzunaBuilder.NonEmptyString where

import qualified Data.Aeson         as Aeson
import qualified Data.List.NonEmpty as NE
import           Data.String

newtype NonEmptyString = NonEmptyString (NE.NonEmpty Char)

instance IsString NonEmptyString where
  fromString str =
    case NE.nonEmpty str of
      Nothing -> error "Invalid argument: Empty string"
      Just x  -> NonEmptyString x

toString :: NonEmptyString -> String
toString (NonEmptyString nonEmptyStr) =
  NE.toList nonEmptyStr

instance Aeson.ToJSON NonEmptyString where
  toJSON (NonEmptyString nonEmptyStr)  =
    Aeson.toJSON $ NE.toList nonEmptyStr
