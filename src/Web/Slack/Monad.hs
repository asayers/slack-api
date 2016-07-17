{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The API exposed in this module is a generalisation of the one in
-- Web.Slack.Internal.
module Web.Slack.Monad
    ( Slack
    , runSlack

    , MonadSlack
    , getConfig
    , getSession
    , getNextEvent
    , sendMessage
    , sendPing
    ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Web.Slack.Internal as I
import Web.Slack.Types
import Web.Slack.WebAPI

class (MonadReader I.SlackHandle m, MonadIO m) => MonadSlack m
instance (MonadReader I.SlackHandle m, MonadIO m) => MonadSlack m

type Slack = ReaderT I.SlackHandle IO

runSlack :: SlackConfig -> Slack a -> IO a
runSlack conf x = I.withSlackHandle conf (runReaderT x)

-- | Retrieve the config used to initiate the session.
getConfig :: MonadSlack m => m SlackConfig
getConfig = I.config <$> ask

-- | When the connection is established, the slack server sends a bunch of
-- session information. This is accessible here.
--
-- (Caveat: this information represents the state as of the beginning of
-- the session; it is liable to become stale. If you care about keeping an
-- up-to-date view of this stuff, you need to track changes to it using
-- 'getNextEvent'.)
getSession :: MonadSlack m => m SlackSession
getSession = I.session <$> ask

-- | Returns the next event in the queue. If none is available, blocks
-- until a new event is recieved.
getNextEvent :: MonadSlack m => m Event
getNextEvent = liftIO . I.nextEvent =<< ask

-- | Send a simple slack message.
sendMessage :: MonadSlack m => ChannelId -> T.Text -> m ()
sendMessage cid msg = ask >>= \h -> liftIO (I.sendMessage h cid msg)

-- | Send a ping to the server, which should respond by sending a 'Pong'
-- event.
sendPing :: MonadSlack m => m ()
sendPing = liftIO . I.sendPing =<< ask
