{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
import Application (makeApplication, cloneRepository) -- for YesodDispatch instance
import qualified Data.Attoparsec.Text as A
import Data.Semigroup ((<>))
import Data.Text (pack)
import Lostation
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import Options.Applicative.Types
import Settings
import System.TimeIt
import Yesod.Core

data Options = Options { remote :: String
                       , privateKeyFile :: String
                       }
  deriving Show

attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . pack)

options :: Parser Options
options = Options
  <$> strOption
     ( long "remote"
     <> metavar "REPO"
     <> help "Git remote for the locks repository" )
  <*> strOption
     ( long "private-key"
     <> metavar "FILE"
     <> help "Private key for the git repository" )

toSettings :: Options -> Settings
toSettings (Options r p) = Settings (pack r) (pack p) (GithubOAuthKeys "" "") "" "" []

main :: IO ()
main = do
  benchOpts <- execParser opts
  print benchOpts
  timeIt $ cloneRepository $ toSettings benchOpts
  print "done"
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Runs El Patrón API service" )
