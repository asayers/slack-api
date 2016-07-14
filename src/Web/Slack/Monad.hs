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
import Web.Slack.Config
import qualified Web.Slack.Internal as I
import Web.Slack.Types

-- | Some useful instances:
--
-- * Slack a
-- * SlackHandle -> IO a
-- * RWST SlackHandle () MyState IO a
--
class (MonadReader I.SlackHandle m, MonadIO m) => MonadSlack m
instance (MonadReader I.SlackHandle m, MonadIO m) => MonadSlack m

type Slack = ReaderT I.SlackHandle IO

runSlack :: SlackConfig -> Slack a -> IO a
runSlack conf x = I.withSlackHandle conf (runReaderT x)

getConfig :: MonadSlack m => m SlackConfig
getConfig = I.config <$> ask

getSession :: MonadSlack m => m SlackSession
getSession = I.session <$> ask

getNextEvent :: MonadSlack m => m Event
getNextEvent = liftIO . I.nextEvent =<< ask

sendMessage :: MonadSlack m => ChannelId -> T.Text -> m ()
sendMessage cid msg = ask >>= \h -> liftIO (I.sendMessage h cid msg)

sendPing :: MonadSlack m => m ()
sendPing = liftIO . I.sendPing =<< ask
