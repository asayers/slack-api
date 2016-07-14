-- | This library contains functionality for writing slack bots which
-- respond to `Event`s sent by the RTM API.
--
-- This basic example echos every message the bot recieves.
-- Other examples can be found in the examples directory.
--
-- > main :: IO ()
-- > main = runSlack myConfig echoBot
-- >
-- > myConfig :: SlackConfig
-- > myConfig = SlackConfig { _slackApiToken = "your API token here" }
-- >
-- > echoBot :: Slack ()
-- > echoBot = do
-- >     event <- getNextEvent
-- >     case event of
-- >         (Message cid _ msg _ _ _) -> sendMessage cid msg
-- >         _ -> return ()
-- >     echoBot
--
module Web.Slack
    ( -- * MonadSlack: The generic API
      MonadSlack
    , getConfig
    , getSession
    , getNextEvent
    , sendMessage
    , sendPing

      -- * Slack: an instance of MonadSlack
    , Slack
    , SlackConfig(..)
    , runSlack

      -- * SlackHandle: for making alternative MonadSlack instances
    , SlackHandle
    , withSlackHandle

      -- * The Slack RTD API
    , Event(..)
    , module Web.Slack.Types
    ) where

import Web.Slack.Monad
import Web.Slack.WebAPI
import Web.Slack.Internal (SlackHandle, withSlackHandle)
import Web.Slack.Types
