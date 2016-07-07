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

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.RWS
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

newtype Slack s a = Slack { runSlack :: RWST SlackHandle () s IO a }
  deriving (Monad, Functor, Applicative, MonadReader SlackHandle, MonadState s, MonadIO)

type SlackBot s =  Event -> Slack s ()

slackLog :: Show a => a -> MonadIO m => m ()
slackLog = liftIO . print

counter :: SlackHandle -> IO Int
counter h = do
    modifyIORef (_shCounter h) (+1)
    readIORef (_shCounter h)
