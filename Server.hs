--Final Project ALP.
--Student: Marcos Pividori

-- | This module shows how to build programs that run on the server.
-- For this, we constructed an Embedded Domain Specific Language (EDSL) on the data type 'Server' a.

module Server(
User,
RegId,
MobileDevice(..),
Position(..),
Server(..),
newMobileDevice,
emptyProgram,
send,
readmessage,
localDevice,
totalRegisteredDevices,
sendWithNotification,
getGps,
getPhoto,
recordStart,
recordStop,
wait,
save,
receiveTextFromMobileDevice,
MessageDevice,
msgNewUrl,
) where

import Text.Parse (runParser,TextParser,parseByRead)
import Data.Time (UTCTime)

type User  = String
type RegId = String
-- | MobileDevice represents a unit defined by your registration code and a user.
data MobileDevice = MobileDevice {user :: String,regId :: String} deriving (Show,Eq)

-- | Represents a ground position, given by the latitude, longitude and the time it was taken.
data Position = Position {lat :: Double,lng :: Double,date :: UTCTime} deriving (Show,Eq)

-- | Creates a MobileDevice, given a user name and registration code.
newMobileDevice :: User -> RegId -> MobileDevice
newMobileDevice = MobileDevice

-- |'Server' build programs that are executed in the server and interact with mobile devices.
data Server a = Send [MobileDevice] String (Server a) -- ^ Represents the sending of a message to mobile devices.
              | Read (String -> MobileDevice -> Server a)-- ^ Represents the pause until the arrival of a message.
              | Interpret (String -> MobileDevice -> Server a)-- ^ Represents the interpretation of a message that has arrived. 
              | LocalDevice (MobileDevice -> Server a)-- ^ Represents the application data to the mobile device that runs the program.
              | TotalRegisteredDevices ([MobileDevice] -> Server a)-- ^ Represents the list of the total registered devices in the Server.
              | Save Position (Server a)-- ^ Represents the operation to keep a position in the database.
              | Error-- ^ Represents an error in some operations.
              | Ret a-- ^ Represents a return value.t


instance Monad Server where
    return x = Ret x
    (Send s1 s2 serv) >>= f = Send s1 s2 (serv >>= f)
    (Read f) >>= g = Read (\xs cel -> (f xs cel) >>= g)
    (Interpret f) >>= g = Interpret (\xs cel -> (f xs cel) >>= g)
    (LocalDevice f) >>= g = LocalDevice (\cel -> (f cel) >>= g)
    (TotalRegisteredDevices f) >>= g = TotalRegisteredDevices (\cels -> (f cels) >>= g)
    (Ret x) >>= g = g x
    Error >>= g = Error
    (Save s serv) >>= g = (Save s (serv >>= g))

-- | 'MessageDevice' represent a message to be sent.
type MessageDevice = String

msgWithNotification :: MessageDevice -> MessageDevice
msgWithNotification xs = "$WITHNOTIFICATION: "++xs

msgGps :: MessageDevice
msgGps = "$GPS"

msgCamera :: MessageDevice
msgCamera = "$CAMERA"

msgRecordStart :: MessageDevice
msgRecordStart = "$RECORDSTART"

msgRecordStop :: MessageDevice
msgRecordStop = "$RECORDSTOP"

msgWait :: Int -> MessageDevice
msgWait time = "$WAIT: " ++ show(time)

msgReceiveText :: MessageDevice
msgReceiveText = "$RECEIVETEXT"

msgNewUrl :: MessageDevice -> MessageDevice
msgNewUrl url = "$NEWURL "++url

-- |Empty Program.
emptyProgram :: Server ()
emptyProgram = Ret ()

-- |'send' sends the message to the list of mobile devices.
send :: [MobileDevice] -> MessageDevice -> Server ()
send cel xs = Send cel xs (return ())

-- |'readmessage' wait for a incoming message.
readmessage :: Server (String,MobileDevice)
readmessage = Read (\xs cel -> return (xs,cel))

-- |'localDevice' gets the information about the mobile device that is running this program.
localDevice :: Server MobileDevice
localDevice = LocalDevice (\x -> return x)

-- |'totalRegisteredDevices' gets a list of the total registered devices in the Server.
totalRegisteredDevices :: Server [MobileDevice]
totalRegisteredDevices = TotalRegisteredDevices (\x -> return x)

-- |'sendWithNotification' sends the message to the list of mobile devices with notification, which means thar the user will be notified about this new message.
sendWithNotification :: [MobileDevice] -> MessageDevice -> Server ()
sendWithNotification cel xs = send cel (msgWithNotification xs)

-- |'getGps' obtains GPS position and returns Nothing (No signal) o Just  (Latitude, Longitude, Date)
getGps :: MobileDevice -> Server (Maybe (Position))
getGps cel = do
             send [cel] msgGps
             (response,_) <- readmessage
             let 
                  pars :: TextParser (Double,Double,UTCTime)
                  pars = parseByRead "(Double,Double,UTCTime)"
                  par = fst (runParser pars response)
             case par of
               Right (lati,lngi,date) -> return $ Just (Position lati lngi date)
               Left _   -> return Nothing

-- |'getPhoto' asks for a photo to the mobile device, which will be stored in the database.
getPhoto :: MobileDevice -> Server ()
getPhoto cel = send [cel] msgCamera

-- |'recordStart' start the audio recording in the mobile device.
recordStart :: MobileDevice -> Server ()
recordStart cel = send [cel] msgRecordStart

-- |'recordStop' stop the audio recording in the mobile device.
recordStop :: MobileDevice -> Server ()
recordStop cel = send [cel] msgRecordStop

-- |'wait' asks the mobile device to wait a number of milliseconds.
wait :: MobileDevice -> Int -> Server ()
wait cel time = do
                   send [cel] (msgWait time)
                   let f = (\_ celu -> if celu == cel 
                                       then return ()
                                       else Read f)
                   Read f

-- |'save' take a position and save it in the database.
save :: Position -> Server ()
save str = Save str (return ())

-- |'receiveTextFromMobileDevice' states that want to receive messages from the console of the mobile devices given.
receiveTextFromMobileDevice :: MobileDevice -> Server ()
receiveTextFromMobileDevice cel = send [cel] msgReceiveText
