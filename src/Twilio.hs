{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, NamedFieldPuns, FlexibleInstances, FlexibleContexts #-}

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
import Control.Failure
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)

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

urlBase :: String
urlBase = "https://api.twilio.com/2010-04-01/Accounts/"

data Method = GET
            | POST
            deriving (Show, Eq)

class IsString a => ToString a where
    toString :: a -> String

instance ToString [Char] where
    toString = id

instance ToString ByteString where
    toString = unsafeCoerce . unpack

twilioReq :: (ToString a, Failure HttpException m) => User -> Method -> a -> m (Request m1)
twilioReq user method path = do
    initReq <- parseUrl $ urlBase <> toString (sid user) <> toString path
    let req = initReq { method = (fromString $ show method), secure = True, responseTimeout = Just 10000000 }
    return $ applyBasicAuth (sid user) (authToken user) req

sendMessage :: User -> SMS -> MaybeT IO SentSMS
sendMessage user sms = MaybeT $ withSocketsDo $ do
    req <- urlEncodedBody [("From", from sms), ("To", to sms), ("Body", body sms)] <$> twilioReq user POST ("/Messages.json" :: String)
    withManager $ \m -> (decode . responseBody) <$> httpLbs req m

queryMessage :: User -> ByteString -> MaybeT IO SentSMS
queryMessage user messageSid = MaybeT $ withSocketsDo $ do
    req <- twilioReq user GET ("/Messages/" <> messageSid <> ".json")
    withManager $ \m -> (decode . responseBody) <$> httpLbs req m
