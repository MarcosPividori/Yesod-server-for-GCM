--Final Project ALP.
--Student: Marcos Pividori

-- |This module presents the configuration of the database used and the types of data stored in it.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

module DataBase where

import Yesod
import Data.Time (UTCTime(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Devices
    usuario String
    password String
    regId String
    UniqueUsuario usuario
    deriving Show
Image
    filename String
    usuario String
    date UTCTime
    deriving Show
Audio
    filename String
    usuario String
    date UTCTime
    deriving Show
Ubicacion
    usuario String
    lati Double
    lngi Double
    datei UTCTime
|]

