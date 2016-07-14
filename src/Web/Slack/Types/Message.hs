{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Web.Slack.Types.Message where

import Data.Aeson.TH
import Data.Char
import qualified Data.Text as T
import Web.Slack.Types.Id

data MessagePayload = MessagePayload
    { messageId      :: Int
    , messageType    :: T.Text
    , messageChannel :: ChannelId
    , messageText    :: T.Text
    } deriving (Show)

data PingPayload = PingPayload
    { pingId        :: Int
    , pingType      :: T.Text
    , pingTimestamp :: Int
    } deriving (Show)

data Attachment = Attachment
    { attachmentTitle :: T.Text
    , attachmentFields :: [Field]
    , attachmentFooter :: T.Text
    }

data Field = Field
    { fieldValue :: T.Text
    , fieldShort :: Bool
    }

$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 7} ''MessagePayload)
$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 4} ''PingPayload)
$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 10} ''Attachment)
$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 5} ''Field)
