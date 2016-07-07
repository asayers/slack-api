{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
------------------------------------------
-- |
-- This module exposes functionality to write bots which responds
-- to `Event`s sent by the RTM API. By using the user state parameter `s`
-- complicated interactions can be established.
--
-- This basic example echos every message the bot recieves.
-- Other examples can be found in the
-- @<http://google.com examples>@ directory.
--
-- > myConfig :: SlackConfig
-- > myConfig = SlackConfig
-- >         { _slackApiToken = "..." -- Specify your API token here
-- >         }
-- >
-- > -- type SlackBot s = Event -> Slack s ()
-- > echoBot :: SlackBot ()
-- > echoBot (Message cid _ msg _ _ _) = sendMessage cid msg
-- > echoBot _ = return ()
-- >
-- > main :: IO ()
-- > main = runBot myConfig echoBot ()
--
module Web.Slack ( runBot
                 -- Re-exports
                 , Slack(..)
                 , SlackBot

                   -- * Low-level API
                 , SlackHandle
                 , withSlack
                 , getEvent
                 , getSession
                 , module Web.Slack.Types
                 , module Web.Slack.Config
                 ) where

import           Control.Lens
import           Control.Monad.RWS
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.IORef
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Network.Socket             as S
import qualified Network.URI                as URI
import qualified Network.WebSockets         as WS
import qualified Network.WebSockets.Stream  as WS
import qualified Network.Wreq               as W
import qualified OpenSSL                    as SSL
import qualified OpenSSL.Session            as SSL
import qualified System.IO.Streams.Internal as StreamsIO
import qualified System.IO.Streams.SSL      as Streams

import           Data.Aeson

import           Web.Slack.Config
import           Web.Slack.State
import           Web.Slack.Types

-- | Run a `SlackBot`. The supplied bot will respond to all events sent by
-- the Slack RTM API.
--
-- Be warned that this function will throw an `IOError` if the connection
-- to the Slack API fails.
runBot :: SlackConfig -> SlackBot s -> s -> IO ()
runBot conf handleEvent initialSt =
    withSlack conf $ \h -> void $ runRWST bot h initialSt
  where
    bot = forever $ runSlack . handleEvent =<< liftIO . getEvent =<< ask

-------------------------------------------------------------------------------
-- Low-level API

withSlack :: SlackConfig -> (SlackHandle -> IO a) -> IO a
withSlack conf fn = do
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


getEvent :: SlackHandle -> IO Event
getEvent h = do
    raw <- WS.receiveData $ _shConnection h
    let (msg :: Either String Event) = eitherDecode raw
    case msg of
      Left e -> do
          BC.putStrLn raw
          putStrLn e
          putStrLn "Please report this failure to the github issue tracker"
          getEvent h
      Right event@(UnknownEvent e) -> do
          print $ e
          putStrLn $ "Failed to parse to a known event"
          putStrLn $ "Please report this failure to the github issue tracker"
          return event
      Right event -> return event

getSession :: SlackHandle -> SlackSession
getSession = _shSession
