{-# LANGUAGE 
    OverloadedStrings
  , DeriveDataTypeable
  , NamedFieldPuns
  , FlexibleInstances
  , FlexibleContexts
  , DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
    
module Twilio where

import Data.ByteString (ByteString, unpack)
import Data.Monoid
import qualified Data.Text as T

import Network (withSocketsDo)
import Network.HTTP.Conduit
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Data
import Data.String
import qualified Data.Text.Encoding as TE

import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow)
import Control.Applicative
import Control.Monad
import Unsafe.Coerce
import Data.Time
import System.Locale
import Data.Maybe
import Control.Monad.Trans.Maybe

data User = User { sid :: ByteString
                 , authToken :: ByteString
                 } deriving (Show, Eq, Data, Typeable)

data SMS = SMS { smsFrom :: ByteString
               , smsTo :: ByteString
               , smsBody :: ByteString
               } deriving (Show, Eq, Data, Typeable)

data SentSMS = SentSMS { smsId :: ByteString
                       , sms :: SMS
                       , smsStatus :: SMSStatus
                       } deriving (Show, Eq, Data, Typeable)

data SMSStatus = Queued
               | Sending
               | Failed
               | Sent UTCTime
               deriving (Show, Eq, Data, Typeable)

data Call = Call { callFrom :: ByteString
                 , callTo :: ByteString
                 , callHandler :: CallHandler
                 } deriving (Show, Eq, Data, Typeable)

-- | What is going to handle the call?
data CallHandler = UrlHandler ByteString  -- ^ Specity a specific URL to quiery for TwilML
                 | ApplicationHandler ByteString  -- ^ Specify a registered application SID.
                 deriving (Show, Eq, Data, Typeable)

data CallRecord = CallRecord { callId :: ByteString
                             } deriving (Show, Eq, Data, Typeable)

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
            _ -> mzero
    parseJSON _ = mzero


instance FromJSON CallRecord where
    parseJSON (Object v) = do
        sid <- v .: "sid"
        return $ CallRecord sid
    parseJSON _ = mzero

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
    
instance FromJSON ByteString  where
    parseJSON = withText "ByteString" $ \u -> pure $ TE.encodeUtf8 u 

class ToParamList a where
    toParamList :: a -> [(ByteString, ByteString)]

instance ToParamList SMS where
    toParamList sms = [("From", smsFrom sms), ("To", smsTo sms), ("Body", smsBody sms)]

instance ToParamList Call where
    toParamList call = [("From", callFrom call), ("To", callTo call)] ++ toParamList (callHandler call)

instance ToParamList CallHandler where
    toParamList (UrlHandler url) = [("Url", url)]
    toParamList (ApplicationHandler appSid) = [("ApplicationSid", appSid)]

twilioReq :: (ToString a, MonadThrow m) => User -> Method -> a -> m (Request )
twilioReq user method path = do
    initReq <- parseUrl $ urlBase <> toString (sid user) <> toString path
    let req = initReq { method = (fromString $ show method), secure = True, responseTimeout = Just 10000000 }
    return $ applyBasicAuth (sid user) (authToken user) req

sendMessage :: MonadIO m => User -> SMS -> MaybeT m SentSMS
sendMessage user sms = MaybeT $ liftIO $ withSocketsDo $ do
    req <- urlEncodedBody (toParamList sms) <$> twilioReq user POST ("/Messages.json" :: String)
    withManager $ \m -> (decode . responseBody) <$> httpLbs req m

queryMessage :: MonadIO m => User -> ByteString -> MaybeT m SentSMS
queryMessage user messageSid = MaybeT $ liftIO $ withSocketsDo $ do
    req <- twilioReq user GET ("/Messages/" <> messageSid <> ".json")
    withManager $ \m -> (decode . responseBody) <$> httpLbs req m

makeCall :: MonadIO m => User -> Call -> MaybeT m CallRecord
makeCall user call = MaybeT $ liftIO $ withSocketsDo $ do
    req <- urlEncodedBody (toParamList call) <$> twilioReq user POST ("/Calls.json" :: String)
    withManager $ \m -> (decode . responseBody) <$> httpLbs req m
