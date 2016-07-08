{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Slack.State where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.IORef
import qualified Network.WebSockets        as WS
import           Web.Slack.Types
import           Web.Slack.Config
import Prelude

data SlackHandle = SlackHandle
    { _shConfig     :: SlackConfig
    , _shConnection :: WS.Connection
    , _shSession    :: SlackSession
    , _shCounter    :: IORef Int
    }

type Slack = ReaderT SlackHandle IO

makeLenses ''SlackHandle

getMessageId :: Slack Int
getMessageId = do
    counter <- view shCounter
    liftIO $ modifyIORef counter (+1)
    liftIO $ readIORef counter

getConfig :: Slack SlackConfig
getConfig = view shConfig

getSession :: Slack SlackSession
getSession = view shSession
