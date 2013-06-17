--Final Project ALP.
--Student: Marcos Pividori

-- |This module joins the rest of the modules and provides the main function: runServer.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

module Principal (runServer) where

import Server
import Storage
import Yesod
import SendGCM
import DataBase
import Base
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Yesod.Static
import qualified Data.Text as T
import Network.HTTP.Conduit (Manager, newManager, def)

{-|Runserver function is the main function.

   It starts the server.

   The program given as argument, will be run by default in the recorded mobile devices. -}

runServer :: Server () -> IO ()
runServer program = do
 almacenam <- new_storage
 runResourceT $ withSqlitePool "DevicesDateBase.db3" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    let sampleStatement = T.concat [ "SELECT ?? ", "FROM  devices "]
    lista <- runSqlPool (rawSql sampleStatement []) pool --obtain the list of registered mobile devices
    let 
        listTotalRegIds = (map (\a -> (devicesRegId(entityVal a))) lista) --obtain the list of regIds
        listUser = (map (\a -> (devicesUsuario(entityVal a))) lista) --obtain the list of users
        rec [] = return ()
        rec (usr:xs) = do
                          establish_new_program usr almacenam emptyProgram -- I establish a new program for each one.
                          rec xs
    liftIO $ do {--This is commented, can be used to alert about your new ip to the mobile devices when you start the server.
                  if listTotalRegIds /= []
                    then do
                           print "Public IP:";
                           url <- getLine 
                           sendGCM (-1) listTotalRegIds (msgNewUrl url)
                           return ()
                    else
                         return ()-}
                  rec listUser
                  man <- newManager def
                  static@(Static settings) <- static "static"
                  warpDebug 3000 $ Messages static pool program almacenam man

openConnectionCount :: Int
openConnectionCount = 10


