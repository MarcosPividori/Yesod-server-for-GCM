--Final Project ALP.
--Student: Marcos Pividori

-- |This module builds the backbone of the server 'Messages'. Define handlers for different orders, and manages the execution of programs written in the EDSL.

{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell,
             QuasiQuotes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, GADTs #-}

module Base (Messages(..)) where

import Server
import Storage
import Yesod
import DataBase
import SendGCM
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as AE
import Data.Text.Internal(empty)
import Yesod.Static
import System.FilePath
import System.Directory (removeFile, doesFileExist, createDirectoryIfMissing)
import Control.Applicative ((<$>), (<*>),pure)
import Data.Text (Text, pack,unpack)
import Yesod.Auth
import Yesod.Auth.GoogleEmail
import Network.HTTP.Conduit (Manager, newManager, def)
import Data.Time (getCurrentTime) 
import Data.Time (UTCTime(..),secondsToDiffTime)
import Data.Time.Calendar
import Data.String
import Settings
import Text.Julius (rawJS)

-- With the data 'Period', I can classify according GPS position time.
data Periodo = Dia | Semana | Mes | DesdeSiempre deriving (Show,Eq,Read)

instance PathPiece Periodo where
    fromPathPiece text = (readMaybe $ unpack text) :: Maybe Periodo
    toPathPiece periodo = fromString $ show periodo

-- The function 'readMaybe' try to read data.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

listavacia = [] -- Useful in templates files.

staticFiles "static"

data Messages = Messages {
                          getStatic :: Static, -- ^ Reference point of the static data.
                          connectionPool :: ConnectionPool, -- ^ Connection to the Database.
                          programa :: (Server ()), -- ^ Program default for mobile devices.
                          estados :: StatesStorage, -- ^ Storage of the state of the running programs.
                          httpManager :: Manager  -- ^ Auth Handler.
                         }

-- We define the routes.
mkYesod "Messages" [parseRoutes|
/ RootR GET
/image/#ImageId ImageR DELETE GET
/audio/#AudioId AudioR GET
/audiodelete/#AudioId AudiodeleteR GET
/userinfo/#Periodo UserinfoR GET
/register RegisterR POST
/fromdevices FromdevicesR POST
/upload UploadR POST
/fromweb FromWebR POST
/locations/#Periodo LocationsR GET
/static StaticR Static getStatic
/auth AuthR Auth getAuth
|]

instance Yesod Messages where
    authRoute _ = Just $ AuthR LoginR 
    isAuthorized a _ =
      case a of
        (AuthR _) -> return Authorized
        (FromdevicesR) -> return Authorized
        (UploadR) -> return Authorized
        (RegisterR) -> return Authorized
        _ -> do 
               mu <- maybeAuthId
               return $ case mu of
                 Nothing -> AuthenticationRequired
                 Just _ -> Authorized
    approot = ApprootStatic "" -- Here you must complete with the correct route.
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        base <- widgetToPageContent ($(widgetFile "base"))
        hamletToRepHtml [hamlet|
$doctype 5
<html>
    <head>
        ^{pageHead base}
        ^{pageHead pc}
    <body>
        ^{pageBody base}
        ^{pageBody pc}
|]

instance YesodAuth Messages where
    type AuthId Messages = Text
    getAuthId = return . Just . credsIdent
    loginDest _ = RootR
    logoutDest _ = RootR
    authPlugins _ =[authGoogleEmail]
    authHttpManager = httpManager
    loginHandler = defaultLayout login

login = do
           addCassius $(cassiusFile "templates/print")
           addCassius $(cassiusFile "templates/main")
           addCassius $(cassiusFile "templates/scheme")
           $(widgetFile "login")

instance YesodPersist Messages where
    type YesodPersistBackend Messages = SqlPersist
    runDB action = do
        Messages _ pool _ _ _ <- getYesod
        runSqlPool action pool

instance RenderMessage Messages FormMessage where
    renderMessage _ _ = defaultFormMessage


--'getRootR' redirects to the home page.
getRootR :: Handler RepHtml
getRootR = do
    maid <- maybeAuthId
    case maid of
               Just a -> redirect $ UserinfoR Dia
               Nothing -> redirect $ AuthR LoginR

--'addStyle' defines certain configurations of the website.
addStyle :: Widget
addStyle = do
    addStylesheetRemote "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.1.0/css/bootstrap-combined.min.css"
    toWidget [lucius|.message { padding: 10px 0; background: #ffffed; } |]
    $(widgetFile "eliminarImagen")

-- 'getUserinfoR' provides the user's home page. (responds to GET requests to '/userinfo/#Periodo')
-- In this, the user can see the mobile device positions, which are shown by a map.
-- Also shows photos / recordings stored and provides the possibility of obtaining a new position and picture / recording .
getUserinfoR :: Periodo -> Handler RepHtml
getUserinfoR periodo = do
      maid <- maybeAuthId
      case maid of
       Nothing -> redirect RootR
       Just a -> do
         persona <- runDB $ getBy $ UniqueUsuario (unpack a)
         case persona of
          Nothing -> defaultLayout $ do
           addCassius $(cassiusFile "templates/print")
           addCassius $(cassiusFile "templates/main")
           addCassius $(cassiusFile "templates/scheme")
           $(widgetFile "loginNotRegistered")
          Just p  -> do
           images <- runDB $ selectList [ImageUsuario ==. (devicesUsuario $ entityVal (p))] [Desc ImageDate]
           audios <- runDB $ selectList [AudioUsuario ==. (devicesUsuario $ entityVal (p))] [Desc AudioDate]
           render <- getUrlRender
           audiosfiles <- return $ formato render audios
           defaultLayout $ do
            addCassius $(cassiusFile "templates/print")
            addCassius $(cassiusFile "templates/main")
            addCassius $(cassiusFile "templates/scheme")
            addStyle
            $(widgetFile "userinfo")
            addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.8/jquery.min.js"
            addScriptRemote "http://www.jplayer.org/latest/js/jquery.jplayer.min.js"
            addScriptRemote "http://www.jplayer.org/latest/js/jplayer.playlist.min.js"
            addScriptRemote "http://www.jplayer.org/latest/js/jquery.jplayer.inspector.js"
            addScriptRemote "http://www.jplayer.org/latest/js/themeswitcher.js"
            $(widgetFile "audioplayer")
            addScriptRemote "http://maps.google.com/maps/api/js?sensor=false"
            addScriptRemote "http://google-maps-utility-library-v3.googlecode.com/svn/trunk/markerclusterer/src/markerclusterer.js"
            $(widgetFile "maps")

-- formato :: (Route Messages -> Text)-> [Entity t1] -> String
-- I use this function to insert the recordings obtained from the BD in the player.
formato _ [] = ""
formato render ((Entity audioId audio):[]) = concat ["{title: \"",show $ audioDate audio,"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=",unpack $ render $ AudiodeleteR audioId,">Eliminar</a>\",mp3:\"",unpack $ render  (AudioR audioId),"\"}"]
formato render ((Entity audioId audio):xs) = concat ["{title: \"",show $ audioDate audio,"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=",unpack $ render $ AudiodeleteR audioId,">Eliminar</a>\",mp3:\"",unpack $ render $ AudioR audioId,"\"},",formato render (xs)]

-- 'getLocationsR' provides the positions of a user stored in the database. (responds to GET requests to '/getlocations/#Periodo')
--  Make a selection of the positions according to the 'Periodo' requested.
getLocationsR :: Periodo -> Handler RepJson
getLocationsR periodo = do
    maid <- maybeAuthId
    case maid of
       Nothing -> redirect RootR
       Just a  -> do
        persona <- runDB $ getBy $ UniqueUsuario (unpack a)
        case persona of
          Nothing -> redirect RootR
          Just p  -> do
           render <- getUrlRender
           cacheSeconds 0
           horaActual <- liftIO $ getCurrentTime
           let time = case periodo of 
                          Dia          -> UTCTime {utctDay = (utctDay horaActual), utctDayTime = (secondsToDiffTime 0) }
                          Semana       -> UTCTime {utctDay = addDays (-6) (utctDay horaActual), utctDayTime = (secondsToDiffTime 0) }
                          Mes          -> let (y,m,_) = toGregorian (utctDay horaActual)
                                              dia = fromGregorian y m 1 
                                          in  UTCTime {utctDay = dia, utctDayTime = (secondsToDiffTime 0) }
                          DesdeSiempre -> UTCTime {utctDay = (fromGregorian 2013 1 1), utctDayTime = (secondsToDiffTime 0) }
           lista <- runDB $ selectList [UbicacionUsuario ==. (devicesUsuario $ entityVal p), UbicacionDatei >=. time] [Desc UbicacionLati,Desc UbicacionLngi,Desc UbicacionDatei]
           jsonToRepJson $ AE.object
             ["locations" .= array (map (go render) lista)]
    where
      go r (Entity _ elem) = AE.object
        [ "lng"  .= (show $ ubicacionLngi elem)
        , "lat"  .= (show $ ubicacionLati elem)
        , "date" .= (show $ ubicacionDatei elem)
        ]
      go _ _ = error "getLocationsR"

-- 'deleteImageR' to delete an image given its ID. (responds to DELETE requests to '/image/#ImageId')
deleteImageR :: ImageId -> Handler ()
deleteImageR imageId = do
    maid <- maybeAuthId
    case maid of
       Nothing -> redirect RootR
       Just a  -> do
        image <- runDB $ get404 imageId
        case (imageUsuario image) == (unpack a) of
            False -> permissionDenied empty
            True  -> do
                        let filename = imageFilename image
                        path <- liftIO $ imageFilePath (imageUsuario image) filename
                        liftIO $ removeFile path
                        stillExists <- liftIO $ doesFileExist path
                        case (not stillExists) of 
                            False  -> redirect RootR
                            True -> do
                                runDB $ delete imageId
                                redirect RootR

-- 'getImageR' to obtain an image given its ID. (responds to GET requests to '/image/#ImageId')
getImageR :: ImageId -> Handler ()
getImageR imageId = do
    maid <- maybeAuthId
    case maid of
       Nothing -> redirect RootR
       Just a  -> do
        imagen <- runDB $ get404 imageId
        case (imageUsuario imagen) == (unpack a) of
            False -> permissionDenied empty
            True  -> do
                     let filename = imageFilename imagen
                     path <- liftIO $ imageFilePath (imageUsuario imagen) filename
                     sendFile typeJpeg path           

-- 'getAudioR' to obtain a reocorging given its ID. (responds to GET requests to '/audio/#AudioId')
getAudioR :: AudioId -> Handler ()
getAudioR audioId = do
    maid <- maybeAuthId
    case maid of
       Nothing -> redirect RootR
       Just a  -> do
        audio <- runDB $ get404 audioId
        case (audioUsuario audio) == (unpack a) of
            False -> permissionDenied empty
            True  -> do
                     let filename = audioFilename audio
                     path <- liftIO $ imageFilePath (audioUsuario audio) filename
                     sendFile typeJpeg path           

-- 'getAudiodeleteR' to delete a reocorging given its ID. (responds to GET requests to '/audiodelete/#AudioId')
getAudiodeleteR :: AudioId -> Handler ()
getAudiodeleteR audioId = do
    maid <- maybeAuthId
    case maid of
       Nothing -> redirect RootR
       Just a  -> do
        audio <- runDB $ get404 audioId
        case (audioUsuario audio) == (unpack a) of
            False -> permissionDenied empty
            True  -> do
                        let filename = audioFilename audio
                        path <- liftIO $ imageFilePath (audioUsuario audio) filename
                        liftIO $ removeFile path
                        stillExists <- liftIO $ doesFileExist path
                        case (not stillExists) of 
                            False  -> redirect RootR
                            True -> do
                                runDB $ delete audioId
                                redirect RootR

-- 'postRegister' allows a mobile device register. (POST messages to '/register')
-- Receives a user name, password and regId provided by a GCM Server.
-- Set it to run the default program on it.
postRegisterR :: Handler ()
postRegisterR = do
    regId <- runInputPost $ ireq textField "regId"
    usr <- runInputPost $ ireq textField "user"
    pass <- runInputPost $ ireq textField "password"
    persona <- runDB $ getBy $ UniqueUsuario (unpack usr)
    case persona of
        Nothing -> do   
                        Messages _ _ p almacenam _ <- getYesod
                        runDB $ insert $ Devices (unpack usr) (unpack pass) (unpack regId)
                        pId <- liftIO $ establish_new_program (unpack usr) almacenam p
                        ejecutar pId "" (newMobileDevice "" "") almacenam
                        sendResponse $ RepJson emptyContent
        Just a	-> case devicesPassword (entityVal (a)) == (unpack pass) of
                       True  -> do
                                  runDB $ update (entityKey (a)) [DevicesRegId =. (unpack regId) ]
                                  sendResponse $ RepJson emptyContent
                       False -> invalidArgs []

-- 'postFromDevicesR' receives the messages sent by registered devices. (POST messages to '/fromdevices')
--  Authenticates devices through its user name and password. 
--  Identifies the running program that is destination of the message and continues the execution of it.
postFromdevicesR :: Handler ()
postFromdevicesR = do
    msg <- runInputPost $ ireq textField "message"
    usr <- runInputPost $ ireq textField "user"
    pass <- runInputPost $ ireq textField "password"
    desti <- runInputPost $ ireq textField "destino"
    dest <- return ((read (unpack desti))::Int)
    Messages _ _ prog almacenam _ <- getYesod
    persona <- runDB $ getBy $ UniqueUsuario (unpack usr)
    case persona of
        Nothing -> permissionDenied empty
	Just a	-> case devicesPassword (entityVal (a)) == (unpack pass) of
                       True  -> if dest < 0 
                                   then do
                                           pId <- liftIO $ establish_new_program (devicesUsuario(entityVal (a))) almacenam prog
                                           ejecutar pId "" (newMobileDevice (devicesUsuario $ entityVal a) (devicesRegId $ entityVal a)) almacenam
                                   else do
                                           ejecutar dest (unpack msg) (newMobileDevice (devicesUsuario $ entityVal a) (devicesRegId $ entityVal a)) almacenam
                       False -> permissionDenied empty

-- 'postUploadR' receives the images and recordings sent from the mobile devices and saves these in the database. (POST messages to '/upload')
postUploadR :: Handler ()
postUploadR = do
   result <- runInputPost $ (,,,)
                  <$> ireq fileField "userfile"
                  <*> ireq textField "user"
                  <*> ireq textField "password"
                  <*> ireq textField "date"
   Messages _ _ _ almacenam _ <- getYesod
   case result of
        (file,user,password,date) -> do
            persona <- runDB $ getBy $ UniqueUsuario (unpack user)
            case persona of
              Nothing -> permissionDenied empty
	      Just a  -> case devicesPassword (entityVal (a)) == unpack password of
                           True  -> do
                                      time <- return ((read (unpack date))::UTCTime)
                                      filename <- writeToServer (unpack user) file
                                      if ((extension filename) == "jpg")
                                      then do
                                              runDB $ insert (Image filename (unpack user) time)
                                              return ()
                                      else do
                                              runDB $ insert (Audio filename (unpack user) time)
                                              return ()
                                      liftIO $ print ("FILE NAME: "++filename)
                                      sendResponse $ RepJson emptyContent
                           False -> permissionDenied empty
        _ -> do
            permissionDenied empty

extension :: String -> String
extension (x:y:z:[]) = (x:y:z:[])
extension (x:xs) = extension xs

-- 'postFromWebR' receives instructions from the website user. (POST messages to '/fromweb')
-- You can order the execution of a program from the Web, such as 'take a photo' or 'get gps'
postFromWebR :: Handler ()
postFromWebR = do
    s    <- runInputPost $ ireq textField "name"
    maid <- maybeAuthId
    case maid of
       Nothing  -> permissionDenied empty
       Just usr -> do
         persona <- runDB $ getBy $ UniqueUsuario (unpack usr)
         case persona of
            Nothing -> permissionDenied empty
	    Just a  -> do
                          Messages _ _ _ almacenam _ <- getYesod
                          pId <- liftIO $ establish_new_program (unpack usr) almacenam $ case (unpack s) of
                                                                                                   "photo" -> (do 
                                                                                                     micel <- localDevice
                                                                                                     getPhoto micel
                                                                                                     return ())
                                                                                                   "startrecording" -> (do 
                                                                                                     micel <- localDevice
                                                                                                     recordStart micel
                                                                                                     return ())
                                                                                                   "stoprecording" -> (do 
                                                                                                     micel <- localDevice
                                                                                                     recordStop micel
                                                                                                     return ())
                                                                                                   "gps"  -> (do 
                                                                                                     micel <- localDevice
                                                                                                     mi_posicion <- getGps micel
                                                                                                     case mi_posicion of
                                                                                                      Nothing  -> return ()
                                                                                                      Just pos -> save pos
                                                                                                     return ())
                          ejecutar pId "" (newMobileDevice (devicesUsuario $ entityVal a) (devicesRegId $ entityVal a)) almacenam
                          redirect RootR

-- 'writeToServer' saves a file in the user's folder.
writeToServer :: User -> FileInfo -> Handler FilePath
writeToServer user file = do
    let filename = unpack $ fileName file
    path <- liftIO $ imageFilePath user filename
    liftIO $ fileMove file path
    return filename

imageFilePath :: User -> String -> IO FilePath
imageFilePath user f = do
                         createDirectoryIfMissing True (uploadDirectory </> user)
                         return (uploadDirectory </> user </> f)

uploadDirectory :: FilePath
uploadDirectory = "datos"

-- 'ejecutar' given a processId, obtains its state and executes it with 'runProgram'.
ejecutar :: ProcessID -> String -> MobileDevice -> StatesStorage -> Handler ()
ejecutar pId entrada cel_origen almacenam = 
                              do
                                 resp <- liftIO $ obtain_program_blocker pId almacenam
                                 case resp of
                                     Nothing         -> permissionDenied empty
                                     Just (user,ser) -> do
                                                          persona <- runDB $ getBy $ UniqueUsuario user
                                                          case persona of
                                                           Nothing -> do
                                                                         liftIO $ print "User not registered"
                                                                         return ()
	                                                   Just a  -> do
                                                            prog <- runProgram pId entrada cel_origen ser (newMobileDevice user (devicesRegId $ entityVal a))
                                                            liftIO $ modify_program_unblocker pId almacenam prog

-- 'runProgram' continues with the execution of a program.
runProgram :: ProcessID -> String -> MobileDevice -> (Server ()) -> MobileDevice -> Handler (Server ())
runProgram pId entrada origen_entrada programa a = 
                               case programa of
                                   (Read f) -> return (Interpret f)
                                   (Interpret f) -> runProgram pId "" origen_entrada (f entrada origen_entrada) a
                                   (Send cels msj serv) -> do
                                                                p <- liftIO $ sendGCM pId (map (\x -> (regId x)) cels) msj
                                                                runProgram pId "" origen_entrada (do p
                                                                                                     serv) a
                                   (LocalDevice f) -> runProgram pId "" origen_entrada (f $ a) a
                                   (TotalRegisteredDevices f) -> do
                                                            lista <- runDB $ selectList [] [Desc DevicesUsuario,Desc DevicesRegId]
                                                            let listaTotal = (map (\a -> newMobileDevice (devicesUsuario(entityVal a)) (devicesRegId(entityVal a))) lista)
                                                            runProgram pId "" origen_entrada (f listaTotal) a
                                   (Error) -> return (Error)
                                   (Ret m) -> do
                                                 return (Ret m)
                                   (Save s serv) -> do
                                                          runDB $ insert $ Ubicacion (user a) (lat s) (lng s) (date s)  
                                                          runProgram pId "" origen_entrada serv a

