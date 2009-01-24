-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Multicast
-- Copyright   :  (c) Audrey Tang 2008
-- License     :  MIT License
-- 
-- Maintainer  :  audreyt@audreyt.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The "Network.Multicast" module is for sending UDP datagrams over multicast
-- (class D) addresses.
--
-----------------------------------------------------------------------------
#include <HsNet.h>

module Network.Multicast (
    -- * Simple sending and receiving
      multicastSender, multicastReceiver
    -- * Additional Socket operations
    , addMembership, dropMembership
    , setLoopbackMode, setTimeToLive, setInterface
    -- * Socket options
    , TimeToLive, LoopbackMode, enableLoopback, noLoopback
) where
import Network.BSD
import Network.Socket
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr

type TimeToLive = Int
type LoopbackMode = Bool

enableLoopback, noLoopback :: LoopbackMode
enableLoopback = True
noLoopback     = False

-- | Calling 'multicastSender' creates a client side UDP socket for sending
-- multicast datagrams to the specified host and port.
--
-- Minimal example:
--
-- > import Network.Socket
-- > import Network.Multicast
-- > main = withSocketsDo $ do
-- >     (sock, addr) <- multicastSender "224.0.0.99" 9999
-- >     let loop = do
-- >         sendTo sock "Hello, world" addr
-- >         loop in loop
--
multicastSender :: HostName -> PortNumber -> IO (Socket, SockAddr)
multicastSender host port = do
    proto <- getProtocolNumber "udp"
    sock  <- socket AF_INET Datagram proto
    addr  <- fmap (SockAddrInet port) (inet_addr host)
    return (sock, addr)

-- | Calling 'multicastReceiver' creates and binds a UDP socket for listening
-- multicast datagrams on the specified host and port.
--
-- Minimal example:
--
-- > import Network.Socket
-- > import Network.Multicast
-- > main = withSocketsDo $ do
-- >     sock <- multicastReceiver "224.0.0.99" 9999
-- >     let loop = do
-- >         (msg, _, addr) <- recvFrom sock 1024
-- >         print (msg, addr) in loop
--
multicastReceiver :: HostName -> PortNumber -> IO Socket
multicastReceiver host port = do
    proto <- getProtocolNumber "udp"
    sock  <- socket AF_INET Datagram proto
#ifdef SO_REUSEPORT
    setSocketOption sock ReusePort 1
#endif
    bindSocket sock $ SockAddrInet port #{const INADDR_ANY}
    addMembership sock host
    return sock

doSetSocketOption :: Storable a => Socket -> a -> IO CInt
doSetSocketOption (MkSocket s _ _ _ _) x = alloca $ \ptr -> do
    poke ptr x
    c_setsockopt s _IPPROTO_IP _IP_MULTICAST_LOOP (castPtr ptr) (toEnum $ sizeOf x)

-- | Enable or disable the loopback mode on a socket created by 'multicastSender'.
-- Loopback is enabled by default; disabling it may improve performance a little bit.
setLoopbackMode :: Socket -> LoopbackMode -> IO ()
setLoopbackMode sock mode = maybeIOError "setLoopbackMode" $ do
    let loop = if mode then 1 else 0 :: CUChar
    doSetSocketOption sock loop
    where

-- | Set the Time-to-Live of the multicast.
setTimeToLive :: Socket -> TimeToLive -> IO ()
setTimeToLive sock ttl = maybeIOError "setTimeToLive" $ do
    let val = toEnum ttl :: CInt
    doSetSocketOption sock val

-- | Set the outgoing interface address of the multicast.
setInterface :: Socket -> HostName -> IO ()
setInterface sock host = maybeIOError "setInterface" $ do
    addr <- inet_addr host
    doSetSocketOption sock addr

-- | Make the socket listen on multicast datagrams sent by the specified 'HostName'.
addMembership :: Socket -> HostName -> IO ()
addMembership s = maybeIOError "addMembership" . doMulticastGroup _IP_ADD_MEMBERSHIP s

-- | Stop the socket from listening on multicast datagrams sent by the specified 'HostName'.
dropMembership :: Socket -> HostName -> IO ()
dropMembership s = maybeIOError "dropMembership" . doMulticastGroup _IP_DROP_MEMBERSHIP s

maybeIOError :: String -> IO CInt -> IO ()
maybeIOError name f = f >>= \err -> case err of
    0 -> return ()
    _ -> ioError (errnoToIOError name (Errno (fromIntegral err)) Nothing Nothing)

doMulticastGroup :: CInt -> Socket -> HostName -> IO CInt
doMulticastGroup flag (MkSocket s _ _ _ _) host = allocaBytes #{size struct ip_mreq} $ \mReqPtr -> do
    addr <- inet_addr host
    #{poke struct ip_mreq, imr_multiaddr} mReqPtr addr
    #{poke struct ip_mreq, imr_interface} mReqPtr (#{const INADDR_ANY} `asTypeOf` addr)
    c_setsockopt s _IPPROTO_IP flag (castPtr mReqPtr) (#{size struct ip_mreq})

#ifdef mingw32_HOST_OS
foreign import stdcall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

foreign import stdcall unsafe "WSAGetLastError"
    wsaGetLastError :: IO CInt

getLastError :: CInt -> IO CInt
getLastError = const wsaGetLastError

_IP_MULTICAST_IF, _IP_MULTICAST_TTL, _IP_MULTICAST_LOOP, _IP_ADD_MEMBERSHIP, _IP_DROP_MEMBERSHIP :: CInt
_IP_MULTICAST_IF    = 9
_IP_MULTICAST_TTL   = 10
_IP_MULTICAST_LOOP  = 11
_IP_ADD_MEMBERSHIP  = 12
_IP_DROP_MEMBERSHIP = 13

#else

foreign import ccall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

getLastError :: CInt -> IO CInt
getLastError = return

_IP_MULTICAST_IF, _IP_MULTICAST_TTL, _IP_MULTICAST_LOOP, _IP_ADD_MEMBERSHIP, _IP_DROP_MEMBERSHIP :: CInt
_IP_MULTICAST_IF    = #const IP_MULTICAST_IF
_IP_MULTICAST_TTL   = #const IP_MULTICAST_TTL
_IP_MULTICAST_LOOP  = #const IP_MULTICAST_LOOP
_IP_ADD_MEMBERSHIP  = #const IP_ADD_MEMBERSHIP
_IP_DROP_MEMBERSHIP = #const IP_DROP_MEMBERSHIP

#endif

_IPPROTO_IP :: CInt
_IPPROTO_IP = #const IPPROTO_IP
