{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module IzunaServer.Service.Github where


import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types              ((.=))
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Client.TLS       as TLS
import           Servant
import qualified Servant.Client                as Client

import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.Type
import           IzunaServer.PullRequest.Model


-- * input


data GithubPullRequestInfoInput = GithubPullRequestInfoInput
    { _prInput_username      :: NonEmptyString Username
    , _prInput_repository    :: NonEmptyString Repo
    , _prInput_pullRequestId :: Nat
    }
    deriving (Show, Generic)

instance Aeson.ToJSON GithubPullRequestInfoInput where
  toJSON GithubPullRequestInfoInput{..} =
    Aeson.object [ "query" .= mkQuery
                 , "variables" .= Aeson.object []
                 ]
    where
      mkQuery :: String
      mkQuery =
        "query { repository(name: \"" <> toString _prInput_repository <> "\", owner: \"" <> toString _prInput_username <> "\") { pullRequest(number: " <> show _prInput_pullRequestId <> ") { commits(last: 10) { nodes { commit { oid } } } baseRef { target { oid } } } } }"


-- * api


type GithubRailsRoutesApi =
  Header "Authorization" String :>
  Header "User-Agent" String :>
  ReqBody '[JSON] GithubPullRequestInfoInput :>
  Post '[JSON] PullRequestInfo


-- * client


getPullRequestInfo :: Text -> GithubPullRequestInfoInput -> IO (Maybe PullRequestInfo)
getPullRequestInfo authorizationToken prInput = do
  manager <- HTTP.newManager TLS.tlsManagerSettings
  Client.runClientM githubRailsRouteClient (clientEnv manager) >>= \case
    Left e ->  do
      putStrLn $ "failed to fetch pull request info for: " <> show prInput <> " - error: " <> show e
      return Nothing
    Right routeContent ->
      return $ Just routeContent
  where
    baseUrl =
      Client.BaseUrl { baseUrlScheme = Client.Https
                     , baseUrlHost = "api.github.com"
                     , baseUrlPort = 443
                     , baseUrlPath = "graphql"
                     }
    githubRailsRouteClient =
      Client.client (Proxy :: Proxy GithubRailsRoutesApi) (Just ("bearer " <> T.unpack authorizationToken)) (Just "servant-user-agent") prInput
    clientEnv manager =
      Client.mkClientEnv manager baseUrl
