module Main where

import Data.Text (pack, unpack, Text)
import Data.UUID (UUID)
import Data.Yaml (FromJSON, decodeEither')
import Options.Applicative
import Options.Applicative.Types
import Text.PDF.Slave.Server.Client

import qualified Data.ByteString as BS
import qualified Data.UUID as UUID

import Server
import Text.PDF.Slave.Server.API

-- | CLI commands
data Command =
  -- | CLI command to render template
  CommandRenderTemplate {
    -- | Path to template file
    renderTemplateFile  :: Text
    -- | Path to template input data
  , renderTemplateInput :: Maybe Text
    -- | Optional overwriting of render id
  , renderTemplateId    :: Maybe APIRenderId
    -- | Name of PDF document where to save result
  , renderDocumentFile  :: Text
    -- | Host of local server for notification target
  , renderHost          :: Text
    -- | Port that is used to receive notification
  , renderPort          :: Int
  }

-- | Command line options
data Options = Options {
  -- | Command to execute
  optionsCommand  :: Command
  -- | Server URL
, optionsUrl      :: Text
  -- | Authorisation login
, optionsLogin    :: Text
  -- | Authorisation password
, optionsPassword :: Text
  -- | Number of seconds an authorisation token is hold
, optionsExpire   :: Word
}

-- | Same as 'strOption' but parses 'Text'
textOption :: Mod OptionFields String -> Parser Text
textOption m = fmap pack $ strOption m

-- | Parse UUID from command line
uuidOption :: Mod OptionFields String -> Parser UUID
uuidOption m = fromM $ do
  uuidStr <- oneM $ strOption m
  case UUID.fromString uuidStr of
    Nothing   -> fail "Failed to parse UUID"
    Just uuid -> return uuid

-- | Parse 'APIRenderId' from command line
renderIdOption :: Mod OptionFields String -> Parser APIRenderId
renderIdOption m = fmap toAPIRenderId $ uuidOption m

-- | Same as 'show' but for 'Text'
showt :: Show a => a -> Text
showt = pack . show

-- | Execute IO action if first argument is Just
whenJust :: Maybe a -> (a -> IO b) -> IO (Maybe b)
whenJust Nothing _ = return Nothing
whenJust (Just a) f = fmap Just $ f a

-- | Parse CLI options
parseOptions :: Parser Options
parseOptions = Options
  <$> commandParser
  <*> textOption (
       long "url"
    <> short 'u'
    <> help "PDF slave server URL"
    <> metavar "SERVER_URL"
    )
  <*> textOption (
       long "login"
    <> short 'l'
    <> help "Login account"
    <> metavar "LOGIN"
    )
  <*> textOption (
       long "password"
    <> short 'p'
    <> help "Password for account"
    <> metavar "PASSWORD"
    )
  <*> pure 1200

-- | Parse CLI command
commandParser :: Parser Command
commandParser = subparser $
    command "render" (info renderParser $ progDesc "Render given template bundle")
  where
    text = fmap pack str

    renderParser = CommandRenderTemplate
      <$> argument text (
           metavar "TEMPLATE_FILE"
        <> help "Template bundle file path"
        )
      <*> optional (textOption $
           long "inputs"
        <> short 'i'
        <> help "Path to optional template inputs"
        <> metavar "TEMPLATE_INPUTS_FILE"
        )
      <*> optional (renderIdOption $
           long "id"
        <> help "Optional id for request, server will use it if specified"
        <> metavar "RENDER_ID"
        )
      <*> argument text (
           metavar "OUTPUT_FILE"
        <> help "Where to save rendered PDF document"
        )
      <*> textOption (
           long "host"
        <> short 'h'
        <> help "Local machine host to receive notification"
        <> metavar "LOCAL_HOST"
        )
      <*> option auto (
           long "port"
        <> short 'p'
        <> help "Local machine port to receive notification"
        <> metavar "LOCAL_PORT"
        )

-- | Reading YAML from file
readYamlFile :: FromJSON a => FilePath -> IO (Either String a)
readYamlFile path = do
  cnt <- BS.readFile path
  return $ case decodeEither' cnt of
    Left e  -> Left $ show e
    Right a -> Right a

-- | Execute program with parsed options
runOptions :: Options -> IO ()
runOptions Options{..} = case optionsCommand of
  CommandRenderTemplate{..} -> do
    -- Reading template
    putStrLn $ "Reading template file " <> show renderTemplateFile
    templateRes <- readYamlFile $ unpack renderTemplateFile
    template <- case templateRes of
      Left e -> fail $ "Failed to parse template file: " <> e
      Right a -> return a
    -- Reading input
    templateInput <- whenJust renderTemplateInput $ \filename -> do
      putStrLn $ "Reading template input file " <> show filename
      inputRes <- readYamlFile $ unpack filename
      case inputRes of
        Left e -> fail $ "Failed to parse input file: " <> e
        Right a -> return a
    -- Executing request
    res <- runPDFSlaveClientM clientOptions $ withAuth optionsLogin optionsPassword optionsExpire $
      renderTemplate template renderTemplateId templateInput
    case res of
      Left er -> putStrLn $ "Failed to put template in rendering queue: " <> show er
      Right i -> do
        putStrLn $ "Template is registered in rendering queue with id: " <> show i
        putStrLn $ "Awaiting notification..."
        nt@APINotificationBody{..} <- waitNotification renderPort
        putStrLn $ "Received notification."
        case (apiNotificationError, apiNotificationDocument) of
          (Just er, _) -> putStrLn $ "Failed to render template:\n" <> unpack er
          (_, Just bs) -> do
            putStrLn $ "Saving result to " <> show renderDocumentFile
            BS.writeFile (unpack renderDocumentFile) bs
          _ -> putStrLn $ "Received malformed notification body: " <> show nt
    where
      clientOptions = PDFSlaveClientConfig {
          pdfSlaveUrl = optionsUrl
        , pdfSlaveReturnUrl = "http://" <> renderHost <> ":" <> showt renderPort -- TODO: Support https
        , pdfSlaveManager = Nothing
        }

main :: IO ()
main = execParser opts >>= runOptions
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "CLI interface for PDF slave server"
     <> header "pdf-slave-server-cli - call remote PDF slave server" )
