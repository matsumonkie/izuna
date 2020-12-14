module IzunaBuilder.NonEmptyString where

import qualified Data.Aeson         as Aeson
import           Data.Function      ((&))
--import qualified Data.Aeson.Types   as Aeson
import qualified Data.List.NonEmpty as NE
import           Data.String
import qualified Data.Text          as T
import qualified Servant.API        as Servant

newtype NonEmptyString a = NonEmptyString (NE.NonEmpty Char) deriving Show

instance IsString (NonEmptyString a) where
  fromString str =
    case NE.nonEmpty str of
      Nothing -> error "Invalid argument: Empty string"
      Just x  -> NonEmptyString x

toString :: NonEmptyString a -> String
toString (NonEmptyString nonEmptyStr) =
  NE.toList nonEmptyStr

instance Servant.FromHttpApiData (NonEmptyString a) where
  parseUrlPiece text =
    case NE.nonEmpty $ T.unpack text of
      Nothing       -> Left "text cannot be empty"
      Just nonEmpty -> Right (NonEmptyString nonEmpty)

instance Aeson.ToJSON (NonEmptyString a) where
  toJSON (NonEmptyString nonEmptyStr) =
    nonEmptyStr & NE.toList & Aeson.toJSON

instance Aeson.FromJSON (NonEmptyString a) where
  parseJSON text = do
    str :: String <- Aeson.parseJSON text
    case NE.nonEmpty str of
      Nothing -> fail "could not parse string to non empty string"
      Just ne -> pure $ NonEmptyString ne
