{-# LANGUAGE RecordWildCards #-}
import Network
import System.IO
import Text.Printf
import Control.Monad
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map

type ClientName = String

-- Client data type
data Client = Client {
    clientName :: ClientName,             -- Client's name
    clientHandle :: Handle,               -- Handle
    clientKicked :: TVar (Maybe String),  -- When kicked string contains reason why
    clientSendChan :: TChan Message       -- Carries all messages sent to a client
    }

-- Message data type
data Message = Notice String              -- Message from server
    | Tell ClientName String              -- Private message from another client
    | Broadcast ClientName String         -- Public  message from another client
    | Command String                      -- Line of text received from the user

-- Server data type
--  mapping from ClientName to Client
data Server = Server {
    clients :: TVar (Map ClientName Client)
    }

-- Port number
port :: Int
port = 5000

-- main function of server
--   creates new server
--   creatkes a socket listening on port
--   forever stays open, accpeting connections not already added
main :: IO ()
main = withSocketsDo $ do
    server <- newServer
    sock <- listenOn (PortNumber (fromIntegral port))
    printf "Listening on port %d\n" port
    forever $ do
        (handle, host, port) <- accept sock
        printf "Accepted connection from %s: %s\n" host (show port)
        forkFinally (talk handle server) (\_ -> hClose handle)

-- Creates a new client
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

-- Creates a new server
newServer :: IO Server
newServer = do
    c <- newTVarIO Map.empty
    return Server { clients = c}

-- Takes a username and attempts to add it as a new client
--  returns Nothing if a client already exists
--  returns Just client if successful.
--  broadcasts the event to all the other connected clients
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

-- Removes a client
removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
    modifyTVar' clients $ Map.delete name
    broadcast server $ Notice (name ++ " has disconnected")

-- Sends a message to a given client
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
    writeTChan clientSendChan msg

-- Broadcasts a message to all clients
broadcast :: Server -> Message -> STM ()
broadcast Server {..} msg = do
    clientmap <- readTVar clients
    mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)

-- Asks user for a name
--  if taken, ask for new name
--  else add to server state
talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
    hSetNewlineMode handle universalNewlineMode
    -- Swallow carriage returns sent by telnet clients
    hSetBuffering handle LineBuffering
    readName
    where
    readName = do
        hPutStrLn handle "What is your name?"
        name <- hGetLine handle
        if null name
        then readName
        else mask $ \restore -> do
            -- add to the server state
            ok <- checkAddClient server name handle
            -- check if client in server
            -- if not ask for another name
            case ok of
                Nothing -> restore $ do
                    hPrintf handle "The name %s is in use, please choose another\n" name
                    readName
                Just client -> restore (runClient server client) `finally` removeClient server name

-- Creates client threads and starts processing events
runClient :: Server -> Client -> IO()
runClient serv@Server{..} client@Client {..} = do
    -- listen to the server
    race server receive
    return ()
    where
    receive = forever $ do
        msg <- hGetLine clientHandle
        atomically $ sendMessage client (Command msg)
    -- check client status
    -- send messages to client
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

-- Handles messages
--   formatting for Notice, Tell, Broadcast and Command
--   /quit : terminates connection with server
--   /_    : unsupported commands
--    _    : message that is broadcasted to all clients
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
