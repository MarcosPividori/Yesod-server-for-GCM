--Final Project ALP.
--Student: Marcos Pividori

-- |This module provides the tools to access the memory representation of the programs running on the Server.

module Storage(
StatesStorage,
ProcessID,
new_storage,
establish_new_program,
obtain_program_blocker,
modify_program_unblocker) where 

import Server (Server(Ret),emptyProgram,User)
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Control.Concurrent.MVar

-- |Code with which we identify a running program.
type ProcessID = Int

type State = (ProcessID,User,Server (),MVar ())

type StatesSet = [State]

-- |Represents the storage of the states of all running programs.
type StatesStorage = IORef (StatesSet,MVar ProcessID)

-- |'new_storage' creates state storage.
new_storage :: IO StatesStorage
new_storage = do
                          m <- newMVar 1
                          r <- newIORef ([],m)
                          return r

-- |'find_and_modify' modifies the program of code 'ProcessID' by the given program.
find_and_modify :: Server () -> ProcessID -> StatesSet -> StatesSet
find_and_modify serv pId [] = []
find_and_modify serv pId ((x,u,s,v):xs) = if x == pId 
                                              then ((x,u,serv,v):xs)
                                              else ((x,u,s,v): find_and_modify serv pId xs)

-- |'obtain_StatesSet' obtains the set of states saved in the storage.
obtain_StatesSet :: StatesStorage -> IO StatesSet
obtain_StatesSet storage = do 
                                       (x,y) <- readIORef storage
                                       return x

-- |'obtain_counter' obtains a new code to identify a new program.
obtain_counter :: StatesStorage -> IO ProcessID
obtain_counter storage = do 
                                       (x,y) <- readIORef storage
                                       cont <- takeMVar y
                                       putMVar y (cont+1)
                                       return cont

-- |'find_State' finds the state of a program, given its code.
find_State :: ProcessID -> StatesSet -> IO (Maybe State) 
find_State pId [] = return Nothing
find_State pId ((x,u,s,v):xs) = if x == pId 
                              then return (Just (x,u,s,v))
                              else find_State pId xs

-- |'find_and_eliminate' remove the program from the set of states.
find_and_eliminate :: ProcessID -> StatesSet -> StatesSet
find_and_eliminate pId [] = []
find_and_eliminate pId ((x,u,s,v):xs) = if x == pId 
                                              then (xs)
                                              else ((x,u,s,v): find_and_eliminate pId xs)

-- |'eliminar_estado' elimine a state. 
elimine_State :: ProcessID -> StatesStorage -> IO () 
elimine_State pId storage = do
                                   atomicModifyIORef storage (\c -> ((find_and_eliminate pId (fst c),snd c),c))
                                   return ()

-- |'obtain_State' obtain the state of a program.
obtain_State :: StatesStorage -> ProcessID -> IO (Maybe State)
obtain_State storage pId = do 
                                  cStates <- obtain_StatesSet storage
                                  find_State pId cStates

-- |'establish_new_program' establish a new running program.
establish_new_program :: User -> StatesStorage -> Server () -> (IO ProcessID)
establish_new_program usr storage program = do
                                     m <- newMVar ()
                                     pId <- obtain_counter storage
                                     atomicModifyIORef storage (\c -> (((pId,usr,program,m):(fst c),snd c),c))
                                     return pId

{-|'obtain_program_blocker' obtain the state of a running program, returns Nothing if it doesnt exist.

    After execution of this function, access to state of the program will be blocked until the function
 'modify_program_unblocker'  is invoked .-}
obtain_program_blocker :: ProcessID -> StatesStorage -> IO (Maybe (String,Server ()))
obtain_program_blocker pId storage = do
                                 p <- obtain_State storage pId
                                 case p of
                                   Just (_,_,_,lock) -> do
                                                                takeMVar lock
                                                                (Just (_,usr,ser,_)) <- obtain_State storage pId
                                                                return (Just (usr,ser))
                                   Nothing                -> return Nothing

-- |'modify_program_unblocker' modifies the state of a program, and unlock the access.
modify_program_unblocker :: ProcessID -> StatesStorage -> Server () -> IO ()
modify_program_unblocker pId storage prog = do
                             case prog of
                               Ret () -> elimine_State pId storage
                               _ -> do
                                 p <- obtain_State storage pId
                                 case p of
                                   Just (pid,user,_,lock) -> do
                                                                atomicModifyIORef storage (\c -> ((find_and_modify prog pId (fst c),snd c),c))
                                                                putMVar lock ()
                                   Nothing                -> return ()

