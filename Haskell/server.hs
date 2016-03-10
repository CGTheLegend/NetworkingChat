module Main where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)

type Msg = (Int, String)

--- Socket Connection
main :: IO ()
main = do
	sock <- socket AF_INET Stream 0           -- create reusable socket
	setSocketOption sock ReuseAddr 1
	bind sock (SockAddrInet 5000 iNADDR_ANY)  -- open TCP connection on port 5000
	listen sock 2
	chan <- newChan
	forkIO $ fix $ \loop -> do
		(_, msg) <- readChan chan
		loop
	mainLoop sock chan 0

--	Accept Connection
--  Run Server Logic
mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
	conn <- accept sock
	forkIO (runConn conn chan msgNum)
	mainLoop sock chan $! msgNum + 1

-- Server Logic
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
	-- setup message, handle and buffer
	let broadcast msg = writeChan chan (msgNum, msg)
	hdl <- socketToHandle sock ReadWriteMode
	hSetBuffering hdl NoBuffering

	-- Prompt user & obtain username
	hPutStrLn hdl "Please enter a username: "
	name <- liftM init (hGetLine hdl)
	broadcast (name ++ " has connected")
	hPutStrLn hdl ("Welcome, " ++ name ++"!")

	-- create new channel available from both locations.
	commLine <- dupChan chan

	-- fork off a thread for reading the duplicated channel
	reader <- forkIO $ fix $ \loop -> do
		(nextNum, line) <- readChan commLine
		when (msgNum /= nextNum) $ hPutStrLn hdl line
		loop

	-- handle messages from client
	handle (\(SomeException _) -> return()) $ fix $ \loop -> do
		line <- liftM init (hGetLine hdl)
		case line of
			"quit" -> hPutStrLn hdl "Bye!"                  -- quit
			_ -> broadcast (name ++ ": " ++ line) >> loop   -- broadcast message

	killThread reader                                   -- kill thread
	broadcast ("<--" ++ name ++ " left.")               -- final broadcast
	hClose hdl                                          -- close handle
