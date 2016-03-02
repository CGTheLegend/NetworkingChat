import Network
import System.IO
import Text.Printf
import Control.Monad
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = withSocketsDo $ do
    server <- newServer
    sock <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port
    forever $ do
        (handle, host, port) <- accept sock
        printf "Accepted connection from %s: %s\n" host (show port)
        forkFinally (talk handle server) (\_ -> hClose handle)
        
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
    clientmap <- readTVar clients
    if Map.member name clientmap
    then return Nothing
    else do 
        client <- newClient name handle
        writeTVar clients $ Map.insert name client clientmap
        broadcast server $ Notice (name ++ " has connected")
        return (Just client)
        
removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
    modifyTVar' clients $ Map.delete name
    broadcast server $ Notice (name ++ " has disconnected")

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
    hSetNewlineMode handle universalNewlineMode
    hsetBuffering handle LineBuffering
    readName
    where
    readName = do
        hPutStrLn handle "What is your name?"
        name <- hGetLine handle
        if null name
        then readName
        else mask $ \restore -> do
            ok <- checkAddClient server name handle
            case ok of
                Nothing -> restore $ do
                    hPrintf handle "The name %s is in use, please choose another\n" name
                    readName
                Just client -> restore (runClient server client) `finally` removeClient server name

port :: Int
port = 5000
