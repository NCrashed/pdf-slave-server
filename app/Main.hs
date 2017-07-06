module Main where

import Control.Lens
import Data.Monoid
import Data.String
import Data.Text (unpack)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Options.Applicative

import Text.PDF.Slave.Server

-- | Argument line options
data Options = Options {
  -- | Path to config, if not set, the app will not start
  configPath :: FilePath
}

-- | Command line parser
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (
         long "conf"
      <> metavar "CONFIG"
      <> help "Path to configuration file"
    )

-- | Execute server with given options
runServer :: Options -> IO ()
runServer Options{..} = do
  cfg <- readConfig configPath
  env <- newServerEnv cfg
  let logger = makeLogger $ serverDetailedLogging cfg
      port = serverPort cfg
      host = fromString . unpack $ serverHost cfg :: HostPreference
      settings = defaultSettings
        & setHost host
        & setPort port
  runSettings settings $ logger $ pdfSlaveServerApp env
  where
    makeLogger b = if b
      then logStdoutDev
      else logStdout

main :: IO ()
main = execParser opts >>= runServer
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Web server for pdf-slave tool"
     <> header "pdf-slave-server - provides web service for pdf-slave tool" )
