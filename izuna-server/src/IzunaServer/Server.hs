{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module IzunaServer.Server(run, mkApp) where

import qualified Control.Monad.Except           as Except
import qualified Control.Monad.IO.Class         as IO
import qualified Control.Monad.Reader           as Reader
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Say
import           Servant                        hiding (BadPassword, NoSuchUser)
import           Servant.API.Flatten            (Flat)

import           IzunaBuilder.NonEmptyString
import           IzunaBuilder.ProjectInfo.Model
import           IzunaBuilder.Type
import           IzunaServer.Project.App

-- * run

run :: IO ()
run = do
  Say.sayString "running izuna-server!"
  app :: Application <- mkApp "" -- <&> cors
  Warp.run 3001 app

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

-- ** project info

type ProjectInfoApi =
  Flat (
    "api" :> "projectInfo"
    :> Capture "username" (NonEmptyString Username)
    :> Capture "repo" (NonEmptyString Repo)
    :> Capture "package" (NonEmptyString Package)
    :> Capture "commit" (NonEmptyString Commit)
    :> Get '[JSON] ModulesInfo
  )

projectInfoServer :: ServerT ProjectInfoApi AppM
projectInfoServer = do
  getProjectInfoHandler

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
