import Control.Concurrent
import Control.Monad
import Data.ByteString.Char8 hiding(head, null)
import Network hiding (accept)
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import System.Environment

main :: IO ()
main = withSocketsDo $ do
    -- create TCP socket at port supplied port or 4242 if no argument was supplied
    args <- getArgs
    serverSocket <- listenOn . PortNumber $ args `headOr` 4242
    -- accept new clients in an endless loop
    forever $ acceptClient serverSocket
    where
      headOr :: [String] -> Int -> PortNumber
      headOr list orValue = fromIntegral (if null list then orValue else read $ head list)


acceptClient :: Socket -> IO ()
acceptClient serverSocket = do
    -- accept new client
    (clientSocket, _) <- accept serverSocket
    -- handle client forever in a separate thread (spawned by 'forkIO')
    let clientHandler = forever $ echo clientSocket
    void . forkIO $ clientHandler

echo :: Socket -> IO ()
echo sock = do
   -- read chunk of max 4096 bytes
   message <- recv sock 4096
   -- respond with 'echo: ' prefix
   let response = pack "echo: " `append` message
   void $ sendAll sock response
