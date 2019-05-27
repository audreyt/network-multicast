import Network.Socket (withSocketsDo)
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recvFrom)
import Network.Multicast

main :: IO ()
main = withSocketsDo $ do
    sock <- multicastReceiver "224.0.0.99" 9999
    let loop = do
        (msg, addr) <- recvFrom sock 1024
        putStrLn $ "Recv [" ++ show addr ++ "]: " ++ C.unpack msg
        loop
    loop
