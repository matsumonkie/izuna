{-# LANGUAGE OverloadedStrings #-}

module DevelMain where

import           Prelude

import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad          ((>=>))
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Typeable
import           Foreign.Store          (Store (..), lookupStore, readStore,
                                         storeAction, withStore)
import           GHC.Word               (Word32)
import           Say
import           System.IO

import           Server

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | Start or restart the server.
-- newStore is from foreign-store.
-- A Store holds onto some data across ghci reloads
update :: IO ()
update = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  putStrLn "Updating"
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- no server running
    Nothing -> do
      putStrLn "No server is running "
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
    -- server is already running
    Just tidStore -> do
      putStrLn "Server is already running "
      restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start

    -- | Start the server in a separate thread.
    start :: MVar () -- ^ Written to when the thread is killed.
          -> IO ThreadId
    start done =
      myThreadId <* (do
                run `catch` \(SomeException e) -> do
                    say "!!! exception in runAppDevel !!!"
                    say $ "X    exception type: " <> tshow (typeOf e)
                    say $ "X    exception     : " <> tshow e
                say "runAppDevel terminated"
            )
            `catch`
            (\(SomeException err) -> do
                say "finally action"
                hFlush stdout
                hFlush stderr
                putMVar done ()
                say $ "Got Exception: " <> tshow err
                throwIO err
            )
            `finally`
            (do
                say "finally action"
                hFlush stdout
                hFlush stderr
                putMVar done ()
                )

-- | kill the server
shutdown :: IO ()
shutdown = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> putStrLn "no app running"
      Just tidStore -> do
          withStore tidStore $ readIORef >=> killThread
          putStrLn "App is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
