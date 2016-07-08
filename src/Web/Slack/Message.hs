{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Web.Slack.Message
    ( sendMessage
    , ping
    ) where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Aeson          (encode)
import           Data.Aeson.TH
import           Data.Char
import qualified Data.Text           as T
import qualified Network.WebSockets  as WS
import           Web.Slack.State
import           Web.Slack.Types
import           Data.Time.Clock.POSIX

import Prelude

data MessagePayload = MessagePayload
                    { messageId      :: Int
                    , messageType    :: T.Text
                    , messageChannel :: ChannelId
                    , messageText    :: T.Text } deriving Show

$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 7} ''MessagePayload)

-- | Send a message to the specified channel.
--
-- If the message is longer than 4000 bytes then the connection will be
-- closed.
sendMessage :: ChannelId -> T.Text -> Slack ()
sendMessage cid message = do
    uid <- getMessageId
    conn <- _shConnection <$> ask
    let payload = MessagePayload uid "message" cid message
    liftIO $ WS.sendTextData conn (encode payload)

data PingPayload = PingPayload
                 { pingId :: Int
                 , pingType :: T.Text
                 , pingTimestamp :: Int
                 } deriving Show

$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 4} ''PingPayload)

-- | Send a ping packet to the server
-- The server will respond with a @pong@ `Event`.
ping :: Slack ()
ping = do
    uid <- getMessageId
    now <- liftIO $ round <$> getPOSIXTime
    conn <- _shConnection <$> ask
    let payload = PingPayload uid "ping" now
    liftIO $ WS.sendTextData conn (encode payload)
