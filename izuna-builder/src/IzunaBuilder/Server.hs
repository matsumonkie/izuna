{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}


module IzunaBuilder.Server(run, mkApp) where

import qualified Control.Monad.Except         as Except
import qualified Control.Monad.IO.Class       as IO
import qualified Control.Monad.Reader         as Reader
--import qualified Data.CaseInsensitive           as CI
--import           Data.Functor                   ((<&>))
--import qualified Network.HTTP.Types.Header      as HTTP
--import qualified Network.HTTP.Types.Method      as HTTP
--import           Network.Wai                    (Middleware)
--import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp     as Warp
import qualified Say
import           Servant                      hiding (BadPassword, NoSuchUser)
import           Servant.Multipart

import           IzunaBuilder.NonEmptyString
--import           IzunaBuilder.ProjectInfo.Model
import           IzunaBuilder.ProjectInfo.App
import           IzunaBuilder.Type

-- * run

run :: Int -> IO ()
run port = do
  Say.sayString $ "running izuna-builder on port: " <> show port
  app :: Application <- mkApp ""
  Warp.run port app

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
    "api"
    :> "projectInfo"
    :> Capture "ghcVersion" (NonEmptyString GhcVersion)
    :> Capture "username" (NonEmptyString Username)
    :> Capture "repo" (NonEmptyString Repo)
    :> Capture "commit" (NonEmptyString Commit)
    :> CaptureAll "projectRoot" String
    :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] ()


-- ** health api

type HealthApi =
  "api" :> "health" :> Get '[JSON] String

healthServer :: ServerT HealthApi AppM
healthServer = do
  return "running!"

projectInfoServer :: ServerT ProjectInfoApi AppM
projectInfoServer = do
  saveProjectInfoHandler

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
