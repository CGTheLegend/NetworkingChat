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
        
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = 
    writeTChan clientSendChan msg
    
newServer :: IO Server
newServer = do
    c <- newTVarIO Map.empty
    return Server { clients = c}
    
broadcast :: Server -> Message -> STM ()
broadcast Server {..} msg = do
    clientmap <- readTVar clients
    mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)

