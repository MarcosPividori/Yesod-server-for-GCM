--Final Project ALP.
--Student: Marcos Pividori

-- | Here I show 4 clear examples. These are programs written with the EDSL.

import Principal
import Server

-- The following program, every so often will get the position of the associated mobile device and the rest of the recorded mobile devices.
-- If it is close to them, it will notify both devices.
program1 :: Server ()
program1 = 
  do
    let recursion = do
          lista <- totalRegisteredDevices
          localdevice <- localDevice
          let rec1 = do
                posicion_local <- getGps localdevice
                if posicion_local == Nothing 
                then rec1
                else return posicion_local
          Just posicion <- rec1
          let 
              lista'= no_localDevice lista localdevice
              rec [] = return ()
              rec (x:xs) = do
                pos <- getGps x
                case pos of
                  Nothing   -> rec xs
                  Just ubic -> do 
                    distancia <- calculate_dist (lat posicion,lng posicion) (lat ubic,lng ubic) 
                    if (distancia  < 100) 
                    then do
                           sendWithNotification [x] (user(localdevice)++" is near, to "++show(distancia)++"m")
                           sendWithNotification [localdevice] (user(x)++" is near, to "++show(distancia)++"m")
                           rec xs
                    else rec xs
          rec lista' -- recorro la lista de celulares, avisando.
          wait localdevice 5000
          recursion
    recursion
    return ()


no_localDevice :: [MobileDevice] -> MobileDevice -> [MobileDevice] 
no_localDevice [] localdevice     = []
no_localDevice (x:xs) localdevice = if x == localdevice
                                    then xs
                                    else (x : (no_localDevice xs localdevice))

-- The following program creates a messenger service.
-- It will wait for incoming messages and resend it to other devices
-- If set as the default program on the server, all recorded programs will have access to the service, writing and receiving messages through the application console.
program2 :: Server ()
program2 = 
 do
   localdevice <- localDevice 
   receiveTextFromMobileDevice localdevice
   let rec = do
               (msj,origen) <- readmessage
               if origen == localdevice
                  then do
                          lista <- totalRegisteredDevices
                          let
                            lista'= no_localDevice lista localdevice
                          sendWithNotification lista' ((user localdevice)++" write: "++msj)
                  else rec
               rec
   rec

-- The following program, every so often will get the GPS position and store it in the database.
program3 :: Server ()
program3 = do
             localdevice <- localDevice
             let  rec = do
                             mi_posicion <- getGps localdevice
                             case mi_posicion of
                                Nothing  -> return ()
                                Just pos -> save pos
                             wait localdevice 20000
                             rec
             rec
             return ()

-- The following program, every so often will get a photo and store it in the database.
program4 :: Server ()
program4 = do
             localdevice <- localDevice
             let  rec1 = do
                           getPhoto localdevice
                           wait localdevice 10000
                           rec1
             rec1
             return ()

-- Calculate the distance in meters from two positions
calculate_dist :: (Double,Double) -> (Double,Double) -> Server Double
calculate_dist (lat1,lon1) (lat2,lon2)= 
  let
     r = 6371
     dLat = (lat2-lat1) * pi / 180
     dLon = (lon2-lon1) * pi / 180
     lat11 = lat1 * pi / 180
     lat22 = lat2 * pi / 180
     a = sin(dLat/2) * sin(dLat/2) + sin(dLon/2) * sin(dLon/2) * cos(lat11) * cos(lat22)
     c = 2 * (atan2  (sqrt a)  (sqrt(1-a)) )
     d = r * c
  in return d

main :: IO ()
main = runServer program2
