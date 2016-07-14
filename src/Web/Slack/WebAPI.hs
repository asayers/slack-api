{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.WebAPI
    ( SlackConfig(..)
    , makeSlackCall

      -- * Some functions from the slack web API
    , rtm_start
    ) where

import Control.Lens
import Control.Monad.Except
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.Wreq as W

-- | Configuration options needed to connect to the Slack API
data SlackConfig = SlackConfig
   { _slackApiToken :: String -- ^ API Token for Bot
   } deriving (Show)

makeLenses ''SlackConfig

makeSlackCall
    :: (MonadError String m, MonadIO m)
    => SlackConfig
    -> String
    -> (W.Options -> W.Options)
    -> m (W.Response BL.ByteString)
makeSlackCall conf method setArgs = do
    let url = "https://slack.com/api/" ++ method
    let setToken = W.param "token" .~ [T.pack (conf ^. slackApiToken)]
    let opts = W.defaults & setToken & setArgs
    resp <- liftIO $ W.getWith opts url
    case resp ^? W.responseBody . key "ok"  . _Primitive of
        Just (BoolPrim False) ->
            let err = resp ^. W.responseBody . key "error" . _String
            in throwError (T.unpack err)
        Just (BoolPrim True)  -> return resp
        _ -> throwError "Couldn't parse response"

rtm_start
    :: (MonadError String m, MonadIO m)
    => SlackConfig
    -> m (W.Response BL.ByteString)
rtm_start conf =
    makeSlackCall conf "rtm.start" id
