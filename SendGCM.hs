--Final Project ALP.
--Student: Marcos Pividori

-- |This module presents the tools to communicate with the GCM Server.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

module SendGCM(sendGCM) where 

import Server
import Storage
import Yesod
import Network.HTTP.Conduit
    (http, parseUrl, withManager, RequestBody (RequestBodyLBS)
    ,requestBody, requestHeaders,	 method, Response (..)
    )
import Data.Aeson.Parser (json)
import Data.Conduit (($$+-))
import Data.Conduit.Attoparsec (sinkParser)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as T
import Data.Map ((!),Map)
import Data.Text (Text, pack,unpack)
import Network.HTTP.Conduit (Manager, newManager, def)

-- |'sendGCM' sends the message to all mobile devices, through a GCM Server.
-- Return: 'Ret' () if all went well, and 'Error' in case of failure.
sendGCM :: ProcessID -> [RegId] -> String -> IO (Server ())
sendGCM pId [] msg = return (Ret ())
sendGCM pId regIds msg = withManager $ \manager -> do
    value <- liftIO $ makeValue pId msg regIds
    let valueBS = AE.encode value
    req' <- liftIO $ parseUrl "https://android.googleapis.com/gcm/send"
    let req = req' { 
                   method = "POST",
                   requestBody = RequestBodyLBS valueBS,
                   requestHeaders = [("Content-Type", "application/json"),("Authorization","key=")]} --Here you must complete with your API Key. (Provided by Google)
    Response status version headers body <- http req manager
    resValue <- body $$+- sinkParser json
    liftIO $ handleResponse resValue

-- 'makeValue' creates the block to be sent.
-- This will consist of a message under the 'data' and the program 'origin' of the message.
makeValue :: ProcessID -> String -> [String] -> IO Value
makeValue pId msg regIds = return $ AE.object
    [ ("registration_ids" .= (regIds :: [String] )),
      ("data" .= AE.object [("data" .= (msg :: String)),("origin" .= ((show pId) :: String))])
    ]

-- 'handleResponse' interprets the response.
-- In case of a failure, it returns a program 'Error'.
handleResponse :: Value -> IO (Server ())
handleResponse v1 = case (T.parseMaybe AE.parseJSON v1) :: Maybe (Map Text Value) of
                        Just a -> case (T.parseMaybe AE.parseJSON (a ! (pack "success"))) :: Maybe Int of
                                      Just 0 -> return Error -- no message arrived safely!
                                      Just _ -> return (Ret ()) -- some message arrived safely!
                                      _ -> return Error
                        _      -> return Error
