{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.Internal
    ( SlackHandle
    , withSlackHandle
    , config
    , session
    , nextEvent
    , sendMessage
    , sendPing
    ) where

import Control.Lens
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.Maybe
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Network.Socket as S
import qualified Network.URI as URI
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import qualified Network.Wreq as W
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import qualified System.IO.Streams.Internal as StreamsIO
import qualified System.IO.Streams.SSL as Streams
import Web.Slack.Config
import Web.Slack.Types

data SlackHandle = SlackHandle
    { _shConfig     :: SlackConfig
    , _shSession    :: SlackSession
    , _shConnection :: WS.Connection
    , _shCounter    :: IORef Int
    }

makeLenses ''SlackHandle

withSlackHandle :: SlackConfig -> (SlackHandle -> IO a) -> IO a
withSlackHandle conf fn = do
    r <- W.get rtmStartUrl
    let Just (BoolPrim ok) = r ^? W.responseBody . key "ok"  . _Primitive
    unless ok $ do
        putStrLn "Unable to connect"
        ioError . userError . T.unpack $ r ^. W.responseBody . key "error" . _String
    let Just url = r ^? W.responseBody . key "url" . _String
    (sessionInfo :: SlackSession) <- case eitherDecode (r ^. W.responseBody) of
        Left e -> print (r ^. W.responseBody) >> (ioError . userError $ e)
        Right res -> return res
    putStrLn "rtm.start call successful"
    let urlErr = error $ "Couldn't parse WebSockets URL: " ++ T.unpack url
    let (host, path) = fromMaybe urlErr $ parseWebSocketUrl (T.unpack url)
    SSL.withOpenSSL $ do
        ctx <- SSL.context
        is  <- S.getAddrInfo Nothing (Just host) (Just $ show port)
        let a = S.addrAddress $ head is
            f = S.addrFamily $ head is
        s <- S.socket f S.Stream S.defaultProtocol
        S.connect s a
        ssl <- SSL.connection ctx s
        SSL.connect ssl
        (i,o) <- Streams.sslToStreams ssl
        (stream :: WS.Stream) <- WS.makeStream  (StreamsIO.read i) (\b -> StreamsIO.write (B.toStrict <$> b) o )
        WS.runClientWithStream stream host path WS.defaultConnectionOptions [] $
            \conn -> do
          freshCounter <- newIORef 0
          let h = SlackHandle
                { _shConfig = conf
                , _shConnection = conn
                , _shSession = sessionInfo
                , _shCounter = freshCounter
                }
          WS.forkPingThread conn 10
          fn h
  where
    port = 443 :: Int
    rtmStartUrl :: String
    rtmStartUrl = "https://slack.com/api/rtm.start?token="
                    ++ (conf ^. slackApiToken)
    parseWebSocketUrl :: String -> Maybe (String, String)
    parseWebSocketUrl url = do
      uri  <- URI.parseURI url
      name <- URI.uriRegName <$> URI.uriAuthority uri
      return (name, URI.uriPath uri)

config :: SlackHandle -> SlackConfig
config = view shConfig

session :: SlackHandle -> SlackSession
session = view shSession

-- | Returns the next event. If the queue is empty, blocks until an event
-- is recieved.
nextEvent :: SlackHandle -> IO Event
nextEvent h = do
    raw <- WS.receiveData $ _shConnection h
    case eitherDecode raw of
        Left e -> do
            putStrLn $ unlines
                [ show raw
                , e
                , "Please report this failure to the github issue tracker"
                ]
            nextEvent h
        Right event@(UnknownEvent val) -> do
            putStrLn $ unlines
                [ show val
                , "Failed to parse to a known event"
                , "Please report this failure to the github issue tracker"
                ]
            return event
        Right event ->
            return event

nextMessageId :: SlackHandle -> IO Int
nextMessageId h = do
    let counter = h ^. shCounter
    liftIO $ modifyIORef counter (+1)
    liftIO $ readIORef counter

-- | Send a message to the specified channel.
--
-- If the message is longer than 4000 bytes then the connection will be
-- closed.
sendMessage :: SlackHandle -> ChannelId -> T.Text -> IO ()
sendMessage h cid message = do
    uid <- nextMessageId h
    let conn = h ^. shConnection
    let payload = MessagePayload uid "message" cid message
    WS.sendTextData conn (encode payload)

-- | Send a ping packet to the server
-- The server will respond with a @pong@ `Event`.
sendPing :: SlackHandle -> IO ()
sendPing h = do
    uid <- nextMessageId h
    now <- round <$> getPOSIXTime
    let conn = h ^. shConnection
    let payload = PingPayload uid "ping" now
    WS.sendTextData conn (encode payload)
