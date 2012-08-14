import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Serialize
import Data.Ethernet

import Foreign.Marshal.Error (void)
import Control.Monad  hiding (void)
import Control.Loop
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.Time.Clock.POSIX
import Network.Pcap hiding (Link)

import Data.Time.TAI64
import Message
import Vindicat
import Atlas
import Network.Pcap.SendReceive
import Crypto.NaCl.Encrypt.PublicKey
import Crypto.NaCl.Key

ethType =  "0xFFFF"
ifnames = ["lo"]
ifmacs = [Mac 0 0 0 0 0 0]

main = do
  kp <- createKeypair
  atlV <- newMVar . mkAtlas $ mkDevice kp (B.pack "nick me") :: IO (MVar Atlas)
  -- start pcap interfaces
  -- only one thread allowed per interface handle / pcap_t
  ifaces <- forM ifnames $ \ifname -> do
    iface <- openLive ifname (read ethType) False 0
    setFilter iface ("ether proto " ++ ethType) True 0 -- not threadsafe in C
    return iface
  -- list of send-receive interfaces for broadcast
  srifsV <- newEmptyMVar
  let broadcast = broadcastUsing ifmacs srifsV :: (Serialize a) => a -> IO ()
  let receiveCallback = receive kp atlV broadcast :: CallbackSRBS
  -- start the send-receive loop!
  putMVar srifsV =<< (sendReceiveLoopBS receiveCallback) `mapM` ifaces
  putStrLn "Started"
  getLine -- wait for other threads

broadcastUsing :: (Serialize a) => [Mac] -> MVar [PcapHandleSR] -> a -> IO ()
broadcastUsing macs srifsV pkt = do
  interfaces <- readMVar srifsV
  forM_ (zip interfaces macs) $ \(iface, mac) -> do
    sendPacketSRBS iface . encode . EthernetFrame bcast mac (read ethType) $ p
  where
  bcast = (Mac 0xff 0xff 0xff 0xff 0xff 0xff)
  p = encode pkt

sendLocalToUsing :: Serialize a => MVar Atlas -> Device -> a -> IO ()
sendLocalToUsing atlasV dev pkt = 

receive :: KeyPair -> MVar Atlas -> (Packet -> IO ()) -> CallbackSRBS
receive kp atlasV broadcast = receivep where
  receivep iface _ rawp = case decode rawp of
    Right (EthernetFrame dest src _ dat) -> case decode dat of
      Right packet -> case packet of
        LinkPack link -> modifyMVar atlasV remember `andif` broadcast packet
          where remember atl = return $ insertLink link atl
        LinkReqPack lrq -> do
          atl <- takeMVar atlasV
          accept_rq <- shouldAcceptLink atl lrq
          if accept_rq
            then do
              let link = acceptLink kp lrq 
                { linkHandle   = Just iface
                , linkOurMac   = Just dest
                , linkOtherMac = Just src
                }
              broadcast . LinkPack $ link
              putMVar atlasV $ fst $ insertLink link atl
            else putMVar atlasV atl
        unknownpacket -> print unknownpacket
      carbage -> print carbage
    carbage -> print carbage

shouldAcceptLink :: Atlas -> Link -> IO Bool
shouldAcceptLink atl lrq = do
  if linkRightEnd lrq `sameDev` ourDev atl
       && linkIsKnown lrq
       && not (linkDead lrq)
       && linkMedium lrq  == Nothing
       && linkIfaceR  lrq == Nothing
    then case linkTime lrq of
      Nothing -> return False
      Just linkTime -> do
        actualTime <- getPOSIXTime
        return $! abs (actualTime - tAI64ToPosix linkTime) < 30
    else return False
