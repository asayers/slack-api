-- | This library contains functionality for writing slack bots which
-- respond to `Event`s sent by the RTM API.
--
-- A basic example of how to use this library is presented below. Other
-- examples can be found in the "examples" directory.
--
module Web.Slack
    ( -- * The generic API
      MonadSlack
    , SlackConfig(..)
    , SlackSession(..)
    , getConfig
    , getSession

      -- * Running your code
      --
      -- The API exposed by this library is fairly generic, and is designed
      -- to support writing code in two different styles: by using a 'Slack'
      -- monad, or by passing around a 'SlackHandle'.
      --
      -- ** Monadic style
      -- $monadic
    , Slack
    , runSlack

      -- ** Handle-based style
      -- $handle
    , SlackHandle
    , withSlackHandle

      -- * The Slack RTD API
      -- $rtdapi
    , getNextEvent
    , sendMessage
    , sendPing
    , Event(..)
    , module Web.Slack.Types

      -- * The Slack Web API
      -- $webapi
    , module Web.Slack.WebAPI
    ) where

import Web.Slack.Monad
import Web.Slack.WebAPI
import Web.Slack.Internal (SlackHandle, withSlackHandle)
import Web.Slack.Types

-- $monadic
--
-- The following example is for a bot which echoes every message it sees
-- back to the channel it case from.
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

-- $handle
--
-- This is the same example as in the previous section, but written using
-- a handle-based style. This approach is more verbose, but can be more
-- flexible (eg. consider forking threads which share a handle). The API
-- exposed by this module can be used with either style.
--
-- > main :: IO ()
-- > main = withSlackHandle myConfig echoBot
-- >
-- > myConfig :: SlackConfig
-- > myConfig = SlackConfig { _slackApiToken = "your API token here" }
-- >
-- > echoBot :: SlackHandle -> IO ()
-- > echoBot h = do
-- >     event <- getNextEvent h
-- >     case event of
-- >         (Message cid _ msg _ _ _) -> sendMessage cid msg h
-- >         _ -> return ()
-- >     echoBot h

-- $rtdapi
--
-- Confusingly, Slack exposes a number of APIs which provide different (but
-- overlapping) functionality. The one primarily exposed by this library is
-- the real-time data (RTD) API. Once the connection is established, the
-- server pushes notifications to the client over a websocket.

-- $webapi
--
-- The Slack Web API is an HTTP REST API. It supports more user operations
-- than the RTD API.
