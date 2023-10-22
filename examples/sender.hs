import Network.Socket (withSocketsDo)
import Network.Socket.ByteString (sendTo)
import qualified Data.ByteString.Char8 as C
import Network.Multicast

import Data.Time.Clock (getCurrentTime)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()
main = withSocketsDo $ do
    (sock, addr) <- multicastSender "224.0.0.99" 9999
    forever $ do
        msg <- fmap show getCurrentTime
        sendTo sock (C.pack msg) addr
        putStrLn $ "Send [" ++ show addr ++ "]: " ++ msg
        threadDelay 1000000
