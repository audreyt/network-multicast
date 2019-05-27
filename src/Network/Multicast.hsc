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
    , ipv6AddMembership, ipv6DropMembership
    , ipv6SetLoopbackMode, ipv6SetHopLimit, ipv6SetInterface
    -- * Socket options
    , HopLimit, TimeToLive, LoopbackMode, enableLoopback, noLoopback
) where
import Data.Maybe
import Network.BSD
import Network.Socket
import Network.Socket.Internal
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Storable
import Foreign.Marshal
import Foreign.Marshal.Utils
import Foreign.Ptr
import Control.Exception (bracketOnError)
import Data.Word (Word32)

type TimeToLive = Int
type HopLimit = Int
type LoopbackMode = Bool

enableLoopback, noLoopback :: LoopbackMode
enableLoopback = True
noLoopback     = False

inet_addr :: HostName -> IO HostAddress
inet_addr = fmap hostAddress . getHostByName

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
    addrInfos <- getAddrInfo Nothing (Just host) Nothing
    case addrInfos of
        addrInfo:_ -> do
            proto <- getProtocolNumber "udp"
            sock <- socket (addrFamily addrInfo) Datagram proto
            case addrAddress addrInfo of
                SockAddrInet _ ha -> return (sock, SockAddrInet port ha)
                SockAddrInet6 _ fi ha si -> return (sock, SockAddrInet6 port fi ha si)
        _ -> fail $ "Cannot use hostName " <> host

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
    addrInfos <- getAddrInfo Nothing (Just host) Nothing
    case addrInfos of
        addrInfo:_ -> bracketOnError get close setup
                      where
                          get :: IO Socket
                          get = do
                            proto <- getProtocolNumber "udp"
                            sock  <- socket (addrFamily addrInfo) Datagram proto
#if defined(SO_REUSEPORT) && ! defined (__linux__)
                            setSocketOption sock ReusePort 1
                            return sock
#else
                            setSocketOption sock ReuseAddr 1
                            return sock
#endif
                          setup :: Socket -> IO Socket
                          setup sock = do
                            case (addrAddress addrInfo) of
                                SockAddrInet _ _ -> do
                                     bind sock $ SockAddrInet port iNADDR_ANY
                                     addMembership sock host Nothing
                                SockAddrInet6 _ _ addr _ -> do
                                     bind sock $ SockAddrInet6 port 0 iN6ADDR_ANY 0
                                     ipv6AddMembership sock addr Nothing
                                _ -> fail "Unsupported address family"
                            return sock
        _ -> fail $ host <> " cannot be resolved as an address"

iNADDR_ANY :: HostAddress
iNADDR_ANY = htonl 0

-- | Converts the from host byte order to network byte order.
foreign import ccall unsafe "htonl" htonl :: Word32 -> Word32


doSetSocketOption :: Storable a => CInt -> Socket -> a -> IO CInt
doSetSocketOption ip_multicast_option sock x = alloca $ \ptr -> do
    poke ptr x
    fd <- fdSocket sock
    c_setsockopt fd _IPPROTO_IP ip_multicast_option (castPtr ptr) (toEnum $ sizeOf x)

doSetSocketOption ip_multicast_option (MkSocket s AF_INET6 _ _ _) x = alloca $ \ptr -> do
    poke ptr x
    c_setsockopt s _IPPROTO_IPV6 ip_multicast_option (castPtr ptr) (toEnum $ sizeOf x)

-- | Enable or disable the loopback mode on a socket created by 'multicastSender'.
-- Loopback is enabled by default; disabling it may improve performance a little bit.
setLoopbackMode :: Socket -> LoopbackMode -> IO ()
setLoopbackMode sock mode = maybeIOError "setLoopbackMode" $ do
    let loop = if mode then 1 else 0 :: CUChar
    doSetSocketOption _IP_MULTICAST_LOOP sock loop

-- | Set the Time-to-Live of the multicast.
setTimeToLive :: Socket -> TimeToLive -> IO ()
setTimeToLive sock ttl = maybeIOError "setTimeToLive" $ do
    let val = toEnum ttl :: CInt
    doSetSocketOption _IP_MULTICAST_TTL sock val

-- | Set the outgoing interface address of the multicast.
setInterface :: Socket -> HostName -> IO ()
setInterface sock host = maybeIOError "setInterface" $ do
    addr <- inet_addr host
    doSetSocketOption _IP_MULTICAST_IF sock addr

-- | Make the socket listen on multicast datagrams sent by the specified 'HostName'.
addMembership :: Socket -> HostName -> Maybe HostName -> IO ()
addMembership s host = maybeIOError "addMembership" . doMulticastGroup _IP_ADD_MEMBERSHIP s host

-- | Stop the socket from listening on multicast datagrams sent by the specified 'HostName'.
dropMembership :: Socket -> HostName -> Maybe HostName -> IO ()
dropMembership s host = maybeIOError "dropMembership" . doMulticastGroup _IP_DROP_MEMBERSHIP s host

-- | Make the socket listen on multicast datagrams sent by the specified 'HostName'.
ipv6AddMembership :: Socket -> HostAddress6 -> Maybe Int -> IO ()
ipv6AddMembership s iface = maybeIOError "ipv6AddMembership" . doMulticastGroup6 _IPV6_ADD_MEMBERSHIP s iface

-- | Stop the socket from listening on multicast datagrams sent by the specified 'HostName'.
ipv6DropMembership :: Socket -> HostAddress6 -> Maybe Int -> IO ()
ipv6DropMembership s iface = maybeIOError "ipv6DropMembership" . doMulticastGroup6 _IPV6_DROP_MEMBERSHIP s iface

-- | Enable or disable the loopback mode on a socket created by 'multicastSender'.
-- Loopback is enabled by default; disabling it may improve performance a little bit.
ipv6SetLoopbackMode :: Socket -> LoopbackMode -> IO ()
ipv6SetLoopbackMode sock mode = maybeIOError "ipv6SetLoopbackMode" $ do
    let loop = if mode then 1 else 0 :: CUInt
    doSetSocketOption _IPV6_MULTICAST_LOOP sock loop

-- | Set the outgoing interface when sending ipv6 multicast datagrams
-- The interface intex can be found from ifNameToIndex (network >= 2.7) or a call to if_nametoindex(3).
ipv6SetInterface :: Socket -> Int -> IO ()
ipv6SetInterface sock iface = maybeIOError "ipv6SetInterface" $ do
    let ifidx = toEnum iface :: CInt
    doSetSocketOption _IPV6_MULTICAST_IF sock ifidx

-- | Set the Hop limit of the multicast.
ipv6SetHopLimit :: Socket -> Int -> IO ()
ipv6SetHopLimit sock ttl = maybeIOError "ipv6SetHopLimit" $ do
    let val = toEnum ttl :: CInt
    doSetSocketOption _IPV6_MULTICAST_HOPS sock val

maybeIOError :: String -> IO CInt -> IO ()
maybeIOError name f = f >>= \err -> case err of
    0 -> return ()
    _ -> ioError (errnoToIOError name (Errno (fromIntegral err)) Nothing Nothing)

doMulticastGroup :: CInt -> Socket -> HostName -> Maybe HostName -> IO CInt
doMulticastGroup flag sock host local = allocaBytes #{size struct ip_mreq} $ \mReqPtr -> do
    addr <- inet_addr host
    iface <- case local of
        Nothing -> return (#{const INADDR_ANY} `asTypeOf` addr)
        Just loc -> inet_addr loc
    #{poke struct ip_mreq, imr_multiaddr} mReqPtr addr
    #{poke struct ip_mreq, imr_interface} mReqPtr iface
    fd <- fdSocket sock
    c_setsockopt fd _IPPROTO_IP flag (castPtr mReqPtr) (#{size struct ip_mreq})

doMulticastGroup6 :: CInt -> Socket -> HostAddress6 -> Maybe Int -> IO CInt
doMulticastGroup6 flag (MkSocket s _ _ _ _) addr iface = allocaBytes #{size struct ipv6_mreq} $ \mReqPtr -> do
    withSockAddr (SockAddrInet6 0 0 addr 0) $ \saddr _ -> do
        copyBytes (#{ptr struct ipv6_mreq, ipv6mr_multiaddr} mReqPtr)
                  (#{ptr struct sockaddr_in6, sin6_addr} saddr)
                  #{size struct in6_addr}
    #{poke struct ipv6_mreq, ipv6mr_interface} mReqPtr (fromMaybe (CUInt 0) (toEnum <$> iface))
    c_setsockopt s _IPPROTO_IPV6 flag (castPtr mReqPtr) (#{size struct ipv6_mreq})

foreign import ccall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

_IP_MULTICAST_IF, _IP_MULTICAST_TTL, _IP_MULTICAST_LOOP, _IP_ADD_MEMBERSHIP, _IP_DROP_MEMBERSHIP :: CInt
_IP_MULTICAST_IF    = #const IP_MULTICAST_IF
_IP_MULTICAST_TTL   = #const IP_MULTICAST_TTL
_IP_MULTICAST_LOOP  = #const IP_MULTICAST_LOOP
_IP_ADD_MEMBERSHIP  = #const IP_ADD_MEMBERSHIP
_IP_DROP_MEMBERSHIP = #const IP_DROP_MEMBERSHIP

_IPV6_MULTICAST_IF, _IPV6_MULTICAST_HOPS, _IPV6_MULTICAST_LOOP, _IPV6_ADD_MEMBERSHIP, _IPV6_DROP_MEMBERSHIP :: CInt
_IPV6_MULTICAST_IF    = #const IPV6_MULTICAST_IF
_IPV6_MULTICAST_HOPS  = #const IPV6_MULTICAST_HOPS
_IPV6_MULTICAST_LOOP  = #const IPV6_MULTICAST_LOOP
_IPV6_ADD_MEMBERSHIP  = #const IPV6_ADD_MEMBERSHIP
_IPV6_DROP_MEMBERSHIP = #const IPV6_DROP_MEMBERSHIP

_IPPROTO_IP :: CInt
_IPPROTO_IP = #const IPPROTO_IP

_IPPROTO_IPV6 :: CInt
_IPPROTO_IPV6 = #const IPPROTO_IPV6
