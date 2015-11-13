import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Network hiding (accept)
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import System.Environment

main :: IO ()
main = withSocketsDo $ do
    -- create TCP socket at port supplied port or 4242 if no argument was supplied
    args <- getArgs
    let port = tryParsePort args `orElse` 4242
    serverSocket <- listenOn $ PortNumber port
    -- accept new clients in an endless loop ('forever')
    forever $ acceptClient serverSocket

acceptClient :: Socket -> IO ()
acceptClient serverSocket = do
    -- accept new client
    (clientSocket, _) <- accept serverSocket
    -- handle client in a separate thread (spawned by 'forkIO')
    -- do this 'forever', but stop when an exception occurs (e.g. because client socket is closed)
    let clientHandler = returnOnException . forever $ echo clientSocket
    void . forkIO $ clientHandler

echo :: Socket -> IO ()
echo sock = do
   -- read chunk of max 4096 bytes
   message <- recv sock 4096
   -- respond with 'echo: ' prefix
   let response = C8.pack "echo: " `C8.append` message
   void $ sendAll sock response

tryParsePort :: [String] -> Maybe PortNumber
tryParsePort (x : _) = Just $ fromIntegral (read x :: Int)
tryParsePort _       = Nothing

orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse _ e        = e

returnOnException :: IO () -> IO ()
returnOnException = handle (\(SomeException _) -> return ())
