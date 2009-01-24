import Network.Socket (withSocketsDo, recvFrom)
import Network.Multicast

main :: IO ()
main = withSocketsDo $ do
    sock <- multicastReceiver "224.0.0.99" 9999
    let loop = do
        (msg, _, addr) <- recvFrom sock 1024
        putStrLn $ "Recv [" ++ show addr ++ "]: " ++ msg
        loop
    loop
