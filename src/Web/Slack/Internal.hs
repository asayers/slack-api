{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Slack.Internal
    ( SlackHandle
    , withSlackHandle
    , config
    , session
    , nextEvent
    , sendMessage
    , sendPing
    ) where

import Control.Error
import Control.Lens hiding ((??))
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import qualified Network.Socket as S
import qualified Network.URI as URI
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import qualified Network.Wreq as W
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import qualified System.IO.Streams.Internal as StreamsIO
import qualified System.IO.Streams.SSL as Streams
import Web.Slack.Types
import Web.Slack.WebAPI

data SlackHandle = SlackHandle
    { _shConfig     :: SlackConfig
    , _shSession    :: SlackSession
    , _shConnection :: WS.Connection
    , _shCounter    :: IORef Int
    }

withSlackHandle :: SlackConfig -> (SlackHandle -> IO a) -> IO a
withSlackHandle conf fn = fromExceptT (ioError . userError) $ do
    r <- makeSlackCall conf "rtm.start" id
    url <- (r ^? W.responseBody . key "url" . _String) ?? "Couldn't parse response"
    sessionInfo <- either throwError return $ eitherDecode (r ^. W.responseBody)
    liftIO $ putStrLn "rtm.start call successful"
    (host, path) <- parseWebSocketUrl (T.unpack url)
    liftIO $ withWebSocket host 443 path $ \conn -> do
        freshCounter <- newIORef 0
        let h = SlackHandle
              { _shConfig = conf
              , _shConnection = conn
              , _shSession = sessionInfo
              , _shCounter = freshCounter
              }
        WS.forkPingThread conn 10
        fn h

parseWebSocketUrl :: Monad m => String -> ExceptT String m (String, String)
parseWebSocketUrl url = do
    uri  <- URI.parseURI url ?? ("Couldn't parse WebSockets URL: " ++ url)
    name <- URI.uriRegName <$> URI.uriAuthority uri ?? ("No authority: " ++ url)
    return (name, URI.uriPath uri)

withWebSocket :: String -> Int -> String -> (WS.Connection -> IO a) -> IO a
withWebSocket host port path fn =
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
        stream <- WS.makeStream
            (StreamsIO.read i)
            (\b -> StreamsIO.write (BL.toStrict <$> b) o)
        WS.runClientWithStream stream host path WS.defaultConnectionOptions [] fn

config :: SlackHandle -> SlackConfig
config = _shConfig

session :: SlackHandle -> SlackSession
session = _shSession

-- | Returns the next event. If the queue is empty, blocks until an event
-- is recieved.
nextEvent :: SlackHandle -> IO Event
nextEvent h@SlackHandle{..} = do
    raw <- WS.receiveData _shConnection
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
nextMessageId SlackHandle{..} = do
    liftIO $ modifyIORef _shCounter (+1)
    liftIO $ readIORef _shCounter

-- | Send a message to the specified channel.
--
-- If the message is longer than 4000 bytes then the connection will be
-- closed.
sendMessage :: SlackHandle -> ChannelId -> T.Text -> IO ()
sendMessage h@SlackHandle{..} cid message = do
    uid <- nextMessageId h
    let payload = MessagePayload uid "message" cid message
    WS.sendTextData _shConnection (encode payload)

-- | Send a ping packet to the server
-- The server will respond with a @pong@ `Event`.
sendPing :: SlackHandle -> IO ()
sendPing h@SlackHandle{..} = do
    uid <- nextMessageId h
    now <- round <$> getPOSIXTime
    let payload = PingPayload uid "ping" now
    WS.sendTextData _shConnection (encode payload)

-------------------------------------------------------------------------------
-- Helpers

fromExceptT :: Monad m => (e -> m a) -> ExceptT e m a -> m a
fromExceptT handle act = runExceptT act >>= either handle return
