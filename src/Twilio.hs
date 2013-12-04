{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, NamedFieldPuns #-}

module Twilio where

import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as L64
import Data.Monoid
import qualified Data.Text as T

import Network (withSocketsDo)
import Network.HTTP.Conduit
import Data.Aeson
import Data.String
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Unsafe.Coerce
import Data.Aeson
import Data.Aeson.Types
import Data.Time
import System.Locale
import Data.Maybe

data User = User { sid :: ByteString
                 , authToken :: ByteString
                 }

data SMS = SMS { from :: ByteString
               , to :: ByteString
               , body :: ByteString
               } deriving (Show)

data SentSMS = SentSMS { smsId :: ByteString
                       , sms :: SMS
                       , smsStatus :: SMSStatus
                       } deriving (Show)

data SMSStatus = Queued
               | Sending
               | Failed
               | Sent UTCTime
               deriving (Show)

twilioUTCFormat :: String
twilioUTCFormat = "%a, %d %b %Y %T %z"

instance FromJSON SentSMS where
    parseJSON (Object v) = do
        sid <- v .: "sid"
        from <- v .: "from"
        to <- v .: "to"
        body <- v .: "body"
        status <- parseJSON (Object v) :: Parser SMSStatus
        return $ SentSMS sid (SMS from to body) status
    parseJSON _ = mzero

instance FromJSON SMSStatus where
    parseJSON (Object v) = do
        statustext <- v .: "status" :: Parser ByteString  -- Whatever intermediate string type you want
        case statustext of
            "queued" -> return Queued
            "sending" -> return Sending
            "failed" -> return Failed
            "sent" -> do
                sentTime <- fromJust . parseTime defaultTimeLocale twilioUTCFormat . T.unpack <$> v .: "date_sent"
                return $ Sent sentTime
    parseJSON _ = mzero

testSMS :: SMS
testSMS = SMS { from = "+441233801290"
              , to = "+447724766810"
              , body = "Test Haskell"
              }

sendMessage :: User -> SMS -> IO (Maybe SentSMS)
sendMessage user sms = withSocketsDo $ do
    initReq <- parseUrl $ "https://api.twilio.com/2010-04-01/Accounts/" <> unsafeCoerce (unpack $ sid user) <> "/Messages.json"
    withManager $ \m -> do
        let req = initReq { method = "POST", secure = True, responseTimeout = Just 10000000 }
        let reqBody = urlEncodedBody [("From", from sms), ("To", to sms), ("Body", body sms)] req
        let reqAuth = (applyBasicAuth (sid user) (authToken user) reqBody)
        liftIO $ print reqAuth
        (decode . responseBody) <$> httpLbs reqAuth m
