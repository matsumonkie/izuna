{-# language BlockArguments #-}

module IzunaBuilder.HieFile.App ( parseHieFiles
                                ) where

-- * import


-- ** base

import Control.Monad.IO.Class ( liftIO )
import Data.Bool
import Data.Foldable
import           Control.Monad         (forM)
import           Prelude               hiding (span)
import           System.Exit           (exitFailure)

-- ** generic-lens
import Data.Generics.Labels ()

-- ** ghc

import           HieBin                (HieFileResult (HieFileResult, hie_file_result),
                                        readHieFileWithVersion)
import           HieTypes              (HieFile, hieVersion)
import           NameCache             (NameCache, initNameCache)
import           UniqSupply            (mkSplitUniqSupply)

-- ** directory

import           System.Directory      (canonicalizePath, doesDirectoryExist,
                                        doesFileExist, doesPathExist,
                                        listDirectory, withCurrentDirectory)

-- ** filepath

import           System.FilePath       (isExtensionOf)

-- * get hie files


parseHieFiles :: [FilePath] -> IO [HieFile]
parseHieFiles hieDirectories = do
  hieFilePaths <-
    concat <$>
      traverse getHieFilePathsIn
        ( if null hieDirectories
          then ["./."]
          else hieDirectories
        )

  nameCache <- do
    uniqSupply <- mkSplitUniqSupply 'z'
    return ( initNameCache uniqSupply [] )

  forM hieFilePaths \hieFilePath -> do
    liftIO $ readCompatibleHieFileOrExit nameCache hieFilePath


-- * get hie files path in


-- | Recursively search for .hie files in given directory
getHieFilePathsIn :: FilePath -> IO [FilePath]
getHieFilePathsIn path = do
  exists <-
    doesPathExist path

  if exists
    then do
      isFile <-
        doesFileExist path

      if isFile && "hie" `isExtensionOf` path
        then do
          path' <-
            canonicalizePath path

          return [ path' ]

        else do
          isDir <-
            doesDirectoryExist path

          if isDir
            then do
              cnts <-
                listDirectory path

              withCurrentDirectory path ( foldMap getHieFilePathsIn cnts )

            else
              return []

    else
      return []


-- * readCompatibleHieFileOrExit


-- | Read a .hie file, exiting if it's an incompatible version.
readCompatibleHieFileOrExit :: NameCache -> FilePath -> IO HieFile
readCompatibleHieFileOrExit nameCache path = do
  res <- readHieFileWithVersion (\ (v, _) -> v == hieVersion) nameCache path
  case res of
    Right ( HieFileResult{ hie_file_result }, _ ) ->
      return hie_file_result
    Left ( v, _ghcVersion ) -> do
      putStrLn $ "incompatible hie file: " <> path
      putStrLn $ "    expected .hie file version " <> show hieVersion <> " but got " <> show v
      putStrLn $ "    HieParser must be built with the same GHC version"
               <> " as the project it is used on"
      exitFailure
