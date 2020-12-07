module BuilderConfig.App ( BuilderConfig(..)
                         , parserInfo
                         ) where

import           Options.Applicative

import           NonEmptyString

data BuilderConfig = BuilderConfig
    { _builderConfig_hieDirectory :: FilePath
    , _builderConfig_user         :: NonEmptyString
    , _builderConfig_repo         :: NonEmptyString
    , _builderConfig_package      :: NonEmptyString
    , _builderConfig_commit       :: NonEmptyString
    , _builderConfig_publicRepo   :: Bool
    }

parserInfo :: ParserInfo BuilderConfig
parserInfo = info (builderConfigParser <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

builderConfigParser :: Parser BuilderConfig
builderConfigParser =
  BuilderConfig
  <$> strOption
  ( long "hie-directory"
    <> metavar "TARGET"
    <> help "Path where your hie files are stored, usually the root of your project, where your stack.yaml or cabal.yml reside"
  )
  <*> strOption
  ( long "user"
    <> metavar "USER"
    <> help "Github user name of this repository"
  )
  <*> strOption
  ( long "repo"
    <> metavar "REPOSITORY"
    <> help "Name of your github repository"
  )
  <*> strOption
  ( long "package"
    <> metavar "PACKAGE"
    <> help "Package name, usually the same as the repository name"
  )
  <*> strOption
  ( long "commit"
    <> metavar "SHA-1"
    <> help "Current commit hash"
  )
  <*> switch
  ( long "public"
    <> help "Whether the repository is public or not"
  )
