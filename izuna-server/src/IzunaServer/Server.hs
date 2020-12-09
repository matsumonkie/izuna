{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module IzunaServer.Server(run, mkApp) where

import qualified Control.Monad.Except           as Except
import qualified Control.Monad.IO.Class         as IO
import qualified Control.Monad.Reader           as Reader
import qualified Data.CaseInsensitive           as CI
import           Data.Functor                   ((<&>))
import qualified Network.HTTP.Types.Header      as HTTP
import qualified Network.HTTP.Types.Method      as HTTP
import           Network.Wai                    (Middleware)
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Middleware.Cors    as Wai
import qualified Say
import           Servant                        hiding (BadPassword, NoSuchUser)
import           Servant.API.Flatten            (Flat)
import           Servant.Multipart

import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.ProjectInfo.Model
import           IzunaBuilder.Type
import           IzunaServer.Project.App

-- * run

run :: IO ()
run = do
  Say.sayString "running izuna-server!"
  app :: Application <- mkApp "" <&> cors
  Warp.run 3001 app

-- * cors

cors :: Middleware
cors =
  Wai.cors onlyRequestForCors
  where
    onlyRequestForCors :: Wai.Request -> Maybe Wai.CorsResourcePolicy
    onlyRequestForCors request =
      case Wai.pathInfo request of
        "cors" : _  ->
          Just Wai.CorsResourcePolicy { Wai.corsOrigins = Just (allowedOrigins, True)
                                      , Wai.corsMethods = [ HTTP.methodPost, HTTP.methodGet, HTTP.methodOptions ]
                                      , Wai.corsRequestHeaders = Wai.simpleResponseHeaders <> allowedRequestHeaders
                                      , Wai.corsExposedHeaders = Nothing
                                      , Wai.corsMaxAge = Nothing
                                      , Wai.corsVaryOrigin = False
                                      , Wai.corsRequireOrigin = True
                                      , Wai.corsIgnoreFailures = False
                                      }
        _ -> Nothing

    allowedOrigins :: [ Wai.Origin ]
    allowedOrigins =
      [ "https://github.com"
      ]

    allowedRequestHeaders :: [ HTTP.HeaderName ]
    allowedRequestHeaders =
      [ "content-type"
      , "x-xsrf-token"
      , "accept"
      , "accept-language"
      , "content-language"
      ] <&> CI.mk

-- * mk app

mkApp :: String -> IO Application
mkApp env = do
  let
    context = EmptyContext
    webApiProxy = Proxy :: Proxy WebApi
  return $
    serveWithContext webApiProxy context $
      hoistServerWithContext webApiProxy (Proxy :: Proxy '[]) (appMToHandler env) apiServer

-- * api

type WebApi =
  ProjectInfoApi :<|> HealthApi

apiServer :: ServerT WebApi AppM
apiServer =
  projectInfoServer :<|> healthServer

-- ** save project info

type ProjectInfoApi =
  Flat (
    "api" :> "projectInfo"
    :> Capture "username" (NonEmptyString Username)
    :> Capture "repo" (NonEmptyString Repo)
    :> Capture "package" (NonEmptyString Package)
    :> Capture "commit" (NonEmptyString Commit)
    :> (
      MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] () :<|>
      Get '[JSON] ProjectInfo
    )
  )

projectInfoServer :: ServerT ProjectInfoApi AppM
projectInfoServer = do
  saveProjectInfoHandler :<|> getProjectInfoHandler

-- ** health api

type HealthApi =
  "api" :> "health" :> Get '[JSON] String

healthServer :: ServerT HealthApi AppM
healthServer = do
  return "running!"



-- * app

newtype AppM a =
  AppM { unAppM :: Except.ExceptT ServerError (Reader.ReaderT String IO) a }
  deriving ( Except.MonadError ServerError
           , Reader.MonadReader String
           , Functor
           , Applicative
           , Monad
           , IO.MonadIO
           )

appMToHandler
  :: String
  -> AppM a
  -> Handler a
appMToHandler env r = do
  eitherErrorOrResult <- IO.liftIO $ flip Reader.runReaderT env . Except.runExceptT . unAppM $ r
  case eitherErrorOrResult of
    Left error   -> throwError error
    Right result -> return result
