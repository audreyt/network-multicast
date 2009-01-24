import Network.Socket (withSocketsDo, sendTo)
import Network.Multicast

import System.Time (getClockTime)
import Control.Concurrent (threadDelay)

main :: IO ()
main = withSocketsDo $ do
    (sock, addr) <- multicastSender "224.0.0.99" 9999
    let loop = do
        msg <- fmap show getClockTime
        sendTo sock msg addr
        putStrLn $ "Send [" ++ show addr ++ "]: " ++ msg
        threadDelay 100000
        loop
    loop
