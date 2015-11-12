import Control.Concurrent
import Control.Monad
import Data.ByteString.Char8
import Network hiding (accept)
import Network.Socket hiding (recv)
import Network.Socket.ByteString

main :: IO ()
main = withSocketsDo $ do
    -- create TCP socket at port 4242
    serverSocket <- listenOn $ PortNumber 4242
    -- accept new clients in an endless loop
    forever $ acceptClient serverSocket

acceptClient :: Socket -> IO ()
acceptClient serverSocket = do
    -- accept new client
    (clientSocket, _) <- accept serverSocket
    -- handle client forever in a separate thread (spawned by 'forkIO')
    let clientHandler = forever $ echo clientSocket
    void . forkIO $ clientHandler

echo :: Socket -> IO ()
echo socket = do
   -- read chunk of max 4096 bytes
   message <- recv socket 4096
   -- respond with 'echo: ' prefix
   let response = pack "echo: " `append` message
   void $ sendAll socket response
