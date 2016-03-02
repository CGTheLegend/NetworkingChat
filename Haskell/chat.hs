{-# LANGUAGE RecordWildCards #-}
import Network
import System.IO
import Text.Printf
import Control.Monad
import Control.Concurrent
import Control.Monad.STM
import Control.Monad.Writer.Class
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map

type ClientName = String

data Client = Client {
    clientName :: ClientName,
    clientHandle :: Handle,
    clientKicked :: TVar (Maybe String),
    clientSendChan :: TChan Message
    }

data Message = Notice String
    | Tell ClientName String
    | Broadcast ClientName String
    | Command String
    
data Server = Server {
    clients :: TVar (Map ClientName Client)
    }

port :: Int
port = 5000    

main :: IO ()
main = withSocketsDo $ do
    server <- newServer
    sock <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port
    forever $ do
        (handle, host, port) <- accept sock
        printf "Accepted connection from %s: %s\n" host (show port)
        forkFinally (talk handle server) (\_ -> hClose handle)

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
    c <- newTChan
    k <- newTVar Nothing
    return Client {
        clientName = name,
        clientHandle = handle,
        clientSendChan = c,
        clientKicked = k
        }
        
newServer :: IO Server
newServer = do
    c <- newTVarIO Map.empty
    return Server { clients = c}

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

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = 
    writeTChan clientSendChan msg
    
broadcast :: Server -> Message -> STM ()
broadcast Server {..} msg = do
    clientmap <- readTVar clients
    mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)
        
talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
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
                
runClient :: Server -> Client -> IO()
runClient serv@Server{..} client@Client {..} = do
    race server receive
    return ()
    where
    receive = forever $ do
        msg <- hGetLine clientHandle
        atomically $ sendMessage client (Command msg)
        
    server = join $ atomically $ do
        k <- readTVar clientKicked
        case k of
            Just reason -> return $
                hPutStrLn clientHandle $ "You have been kicked: " ++ reason
            Nothing -> do
                msg <- readTChan clientSendChan
                return $ do
                    continue <- handleMessage serv client msg
                    when continue $ server
                            
handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message = 
    case message of
        Notice msg -> output $ "*** " ++ msg
        Tell name msg -> output $ "*"  ++ name ++ "*: " ++ msg
        Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
        Command msg ->
            case words msg of
                ["/quit"] -> 
                    return False
                ('/':_):_ -> do
                    hPutStrLn clientHandle $ "Unrecognized command: " ++ msg
                    return True
                _ -> do
                    atomically $ broadcast server $ Broadcast clientName msg
                    return True
    where
        output s = do hPutStrLn clientHandle s; return True


