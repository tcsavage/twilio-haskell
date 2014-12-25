twilio-haskell
==============

Send SMS easily: 
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans.Maybe
import Twilio

main :: IO ()
main =
    let u = User "Your Twilio User ID" "Your Twilio Auth Secret"
        send = sendMessage u
        message = "Boo"
        from = "+18001234567"
        to = "+19001234567"
    in
        do
          status <- runMaybeT $ send $ SMS from to message
          putStrLn $ show status
```
