{-# LANGUAGE DataKinds #-}

module IzunaServer.PullRequest.App where

import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
import qualified Control.Monad.IO.Class        as IO
import           Control.Monad.Reader          (MonadReader)
import qualified Control.Monad.Reader          as Reader
import           Data.Functor                  ((<&>))
import qualified Servant

import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.Type
import           IzunaServer.Env
import           IzunaServer.PullRequest.Model
import qualified IzunaServer.Service.Github    as Github


pullRequestInfoHandler
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError Servant.ServerError m
     )
  => NonEmptyString Username
  -> NonEmptyString Repo
  -> Nat
  -> m PullRequestInfo
pullRequestInfoHandler username repo pullRequestId = do
  authorizationToken <- Reader.ask <&> _env_githubAuthToken
  mGithubPullRequestInfo <- IO.liftIO $ Github.getPullRequestInfo authorizationToken $ Github.GithubPullRequestInfoInput
    { _prInput_username = username
    , _prInput_repository = repo
    , _prInput_pullRequestId = pullRequestId
    }
  case mGithubPullRequestInfo of
    Just pullRequestInfo -> return pullRequestInfo
    Nothing              -> Servant.throwError Servant.err404
