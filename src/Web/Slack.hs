-- | This library contains functionality for writing slack bots. A basic
-- example of how to use this library is presented below. Other examples
-- can be found in the "examples" directory.
--
-- > main :: IO ()
-- > main = runSlack myConfig echoBot
-- >
-- > myConfig :: SlackConfig
-- > myConfig = SlackConfig { _slackApiToken = "your API token here" }
-- >
-- > -- | For all channels of which this bot is a member, it simply watches
-- > -- for messages and echoes them back to the channel they came from.
-- > echoBot :: Slack ()
-- > echoBot = do
-- >     event <- getNextEvent
-- >     case event of
-- >         Message cid _ msg _ _ _ -> sendMessage cid msg
-- >         _ -> return ()
-- >     echoBot
--
-- Confusingly, Slack exposes a number of APIs which provide different (but
-- overlapping) functionality. This library is mostly about the RTD API,
-- but it also has some very limited support for using the Web API.
--
module Web.Slack
    ( -- * Writing bots
      MonadSlack
    , getConfig

      -- ** The Slack RTD API
      -- $rtdapi
    , getNextEvent
    , getSession
    , sendMessage
    , sendPing

      -- ** The Slack Web API
      -- $webapi
    , sendRichMessage
    , module Web.Slack.WebAPI

      -- * Running your code
      -- $running_code

      -- ** Monadic style
      -- $monadic
    , Slack
    , runSlack

      -- ** Handle-based style
      -- $handle
    , SlackHandle
    , withSlackHandle

      -- * Types
    , SlackConfig(..)
    , SlackSession(..)
    , Event(..)
    , module Web.Slack.Types
    ) where

import Web.Slack.Monad
import Web.Slack.WebAPI
import Web.Slack.Internal (SlackHandle, withSlackHandle)
import Web.Slack.Types

-- $rtdapi
--
-- The Slack Real-Time Data API. Once the connection is established, the
-- server pushes notifications to the client over a websocket. You can
-- recieve these notifications by calling 'getNextEvent'.
--
-- The RTD API also allows the client to send some messages back over the
-- websocket connection. At the moment, it only really allows the sending
-- of simple messages. For more complex interactions, you still have to use
-- the Web API (see below).

-- $webapi
--
-- The Slack Web API is an HTTP REST API. It supports more user operations
-- than the RTD API. This API is still not very well supported by this
-- library.

-- $running_code
--
-- The functions exposed by this library require access to a resource which
-- must be acquired: namely, a websocket connection. Such a resource is
-- normally represented by a handle. However, since the user typically
-- deals with just one such value, it is also convenient to hide it in the
-- monadic context.
--
-- This library is designed to support both styles: you can use the 'Slack'
-- monad, or explicitly pass around a 'SlackHandle'. In fact, so long as
-- the base monad is IO and we have access to a SlackHandle, the functions
-- above should work. We use 'MonadSlack', of which both @Slack a@ and
-- @SlackHandle -> IO a@ are instances. The user may find other instances
-- such as @RWST SlackHandle () MyState IO@ useful.

-- $monadic
--
-- This is a slightly more consise version of the example at the top this
-- page (the "echobot"), again written using the 'Slack' monad.
--
-- > main :: IO ()
-- > main = runSlack myConfig echobot
-- >
-- > echobot :: Slack ()
-- > echobot = forever $ getNextEvent >>= \case
-- >     Message cid _ msg _ _ _ -> sendMessage cid msg
-- >     _ -> return ()

-- $handle
--
-- This is the same example as in the previous section, but written using
-- a handle-based style. This approach is more verbose, but can be more
-- flexible (eg. consider forking threads which share a handle).
--
-- > main :: IO ()
-- > main = withSlackHandle myConfig echobot
-- >
-- > echobot :: SlackHandle -> IO ()
-- > echobot h = forever $ getNextEvent h >>= \case
-- >     Message cid _ msg _ _ _ -> sendMessage cid msg h
-- >     _ -> return ()
