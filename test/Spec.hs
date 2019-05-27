import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe
import Network.Multicast
import Network.Socket
import Test.Hspec


timeout :: Int -> IO a -> IO (Maybe a)
timeout t act = do
  r <- newEmptyMVar
  actTid <- forkIO $ act >>= \v -> putMVar r (Just v)
  waitTid <- forkIO $ threadDelay t >> putMVar r Nothing
  v <- takeMVar r
  mapM killThread [actTid, waitTid]
  return v

main :: IO ()
main = hspec $ do
  describe "ipv6" $ do
    it "can create, send and receive multicast data" $ do
      socketReady <- newEmptyMVar
      messageContent <- newEmptyMVar

      -- Create a receiver process
      forkIO $ do
        sock <- multicastReceiver "ff01::42" 1234
        putMVar socketReady ()
        timeout 10000 $
          recvFrom sock 1024 >>=
            \(msg, _, _) -> putMVar messageContent (Just msg)
        -- Make sure that a value is present in the MVar
        tryPutMVar messageContent Nothing
        close sock
        

      -- Once the receiver is ready, starts a sender process
      _ <- takeMVar socketReady
      forkIO $ do
        (sock, addr) <- multicastSender "ff01::42" 1234
        -- make sure this packet never goes through a router,
        -- the ff01 scope is supposed to be interface-local anyway
        ipv6SetHopLimit sock 0
        sendTo sock "Hello world" addr
        close sock
     
      --
      msg <- takeMVar messageContent
      msg `shouldSatisfy` (not . isNothing)
      case msg of
        Just dat -> dat `shouldBe` "Hello world"

    it "properly handles loopback multicast mode" $ do
      socketReady <- newEmptyMVar
      messageContent <- newEmptyMVar

      -- create the receiver process
      forkIO $ do
        sock <- multicastReceiver "ff01::42" 1234
        putMVar socketReady ()
        timeout 10000 $
          recvFrom sock 1024 >>=
            \(msg, _, _) -> putMVar messageContent (Just msg)
        -- Make sure that a value is present in the MVar
        tryPutMVar messageContent Nothing
        close sock
        

      -- create the sender process
      _ <- takeMVar socketReady
      forkIO $ do
        (sock, addr) <- multicastSender "ff01::42" 1234
        -- make sure this packet never goes through a router,
        -- the ff01 scope is supposed to be interface-local anyway
        ipv6SetHopLimit sock 0
        -- Asks the socket to only send data out
        ipv6SetLoopbackMode sock False
        sendTo sock "Hello world" addr
        close sock

      -- We expect that with the ipv6SetLoopbackMode False,
      -- the packet has only gone out, and then should not
      -- have been received locally
      msg <- takeMVar messageContent
      msg `shouldSatisfy` isNothing

  describe "ipv4" $ do
    it "can create, send and receive multicast data" $ do
      socketReady <- newEmptyMVar
      messageContent <- newEmptyMVar

      -- Create a receiver process
      forkIO $ do
        sock <- multicastReceiver "224.0.0.99" 1234
        putMVar socketReady ()
        timeout 10000 $
          recvFrom sock 1024 >>=
            \(msg, _, _) -> putMVar messageContent (Just msg)
        -- Make sure that a value is present in the MVar
        tryPutMVar messageContent Nothing
        close sock
        

      -- Once the receiver is ready, starts a sender process
      _ <- takeMVar socketReady
      forkIO $ do
        (sock, addr) <- multicastSender "224.0.0.99" 1234
        setTimeToLive sock 0
        sendTo sock "Hello world" addr
        close sock
     
      --
      msg <- takeMVar messageContent
      msg `shouldSatisfy` (not . isNothing)
      case msg of
        Just dat -> dat `shouldBe` "Hello world"

    it "properly handles loopback multicast mode" $ do
      socketReady <- newEmptyMVar
      messageContent <- newEmptyMVar

      -- create the receiver process
      forkIO $ do
        sock <- multicastReceiver "224.0.0.99" 1234
        putMVar socketReady ()
        timeout 10000 $
          recvFrom sock 1024 >>=
            \(msg, _, _) -> putMVar messageContent (Just msg)
        -- Make sure that a value is present in the MVar
        tryPutMVar messageContent Nothing
        close sock
        

      -- create the sender process
      _ <- takeMVar socketReady
      forkIO $ do
        (sock, addr) <- multicastSender "224.0.0.99" 1234
        setTimeToLive sock 0
        -- Asks the socket to only send data out
        setLoopbackMode sock False
        sendTo sock "Hello world" addr
        close sock

      -- We expect that with the setLoopbackMode False,
      -- the packet has only gone out, and then should not
      -- have been received locally
      msg <- takeMVar messageContent
      msg `shouldSatisfy` isNothing
