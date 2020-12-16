{-# LANGUAGE DeriveGeneric #-}

module IzunaServer.PullRequest.Model where

-- * import

-- ** vector

import qualified Data.Vector                 as Vector

-- ** base

import qualified Control.Monad               as Monad
--import           Data.Function               ((&))
import           Data.Functor                ((<&>))

-- ** ghc

import           GHC.Generics

-- ** non empty

import qualified Data.List.NonEmpty          as NE

-- ** aeson

import qualified Data.Aeson                  as Aeson
import           Data.Aeson.Types            (Object, Parser, (.:))

-- ** local

import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.Type


-- * model


data PullRequestInfo = PullRequestInfo
    { _pullRequestInfo_targetOid  :: NonEmptyString CommitId
    , _pullRequestInfo_commitOids :: NonEmpty (NonEmptyString CommitId)
    }
    deriving (Generic)

instance Aeson.FromJSON PullRequestInfo where
  parseJSON = Aeson.withObject "PullRequestInfo" $ \o -> do
    _pullRequestInfo_targetOid <- o .: "data" .-> "repository" .-> "pullRequest" .-> "baseRef" .-> "target" .-> "oid"
    nodes :: Aeson.Value <- o .: "data" .-> "repository" .-> "pullRequest" .-> "commits" .-> "nodes"
    _pullRequestInfo_commitOids <-  Aeson.withArray "" arrayParser nodes
    return $ PullRequestInfo{..}
      where
        arrayParser :: Aeson.Array -> Parser (NonEmpty (NonEmptyString CommitId))
        arrayParser array =
          Monad.mapM nodeParser array <&> Vector.toList <&> NE.fromList

        nodeParser :: Aeson.Value -> Parser (NonEmptyString CommitId)
        nodeParser = Aeson.withObject "Node" $ \o -> do
          o .: "commit" .-> "oid"

        (.->) :: Aeson.FromJSON a => Parser Object -> Text -> Parser a
        (.->) parser key = do
          obj <- parser
          obj .: key


instance Aeson.ToJSON PullRequestInfo where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop $ length ("_pullRequestInfo_" :: String) }
