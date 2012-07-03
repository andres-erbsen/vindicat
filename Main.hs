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
import Network.Pcap

import Message
import Vindicat
import Atlas
import Network.Pcap.SendReceive
import Crypto.NaCl.Encrypt.PublicKey
import Crypto.NaCl.Key

initAtlas kp = do
  let ourDev = mkDevice kp (Mac 0 0 0 0 0 0) (B.pack "CHANGETHIS")
  newMVar $ mkAtlas ourDev

ethType =  "0xFFFF"

main = do
  kp <- createKeypair
  atlV <- initAtlas kp
  -- start pcap interfaces
  ifaces <- forM ["lo"] $ \ifname -> do
    -- only one thread allowed per interface handle / pcap_t
    iface <- openLive ifname (read ethType) False 0
    setFilter iface ("ether proto " ++ ethType) True 0 -- not threadsafe in C
    return iface
  srifsV <- newEmptyMVar -- list send-receive interfaces for broadcast
  let broadcast = broadcastUsing srifsV
  let receiveCallback = receive kp atlV broadcast
  -- start the send-receive loop!
  putMVar srifsV =<< (sendReceiveLoopBS receiveCallback) `mapM` ifaces
  putStrLn "Started"
  getLine -- wait for other threads

broadcastUsing :: (Serialize a) => MVar [PcapHandleSR] -> a -> IO ()
broadcastUsing srifsV pkt = do
  interfaces <- readMVar srifsV
  forM_ interfaces $ \iface -> do
   let ethpkt = encode pkt
   sendPacketSRBS iface ethpkt

receive :: KeyPair -> MVar Atlas -> (Packet -> IO ()) -> CallbackSRBS
receive kp atlasV broadcast = receivep where
  receivep iface _ rawp = case decode rawp of
    Right etherp -> case decode (etherData etherp) of
      Right packet -> case packet of
        DevicePack dev -> modifyMVar atlasV remember `andif` broadcast packet
          where remember atl = return $ insertDevice dev atl
        LinkPack  link -> modifyMVar atlasV remember `andif` broadcast packet
          where remember atl = return $ insertLink link atl
        LinkReqPack lh -> do
          atl <- readMVar atlasV
          let other = getDevice atl (linkHalfLeftEnd lh)
          when (shouldAcceptLink atl lh) $ do
            modifyMVar atlasV remember
            broadcast . LinkPack $ link
            where
              remember atl = return $ insertLink link atl
              link = acceptLink kp lh  
        FwdPack tid bs -> return ()
        DataPacket  da -> putStrLn $ "> " ++ show da
        carbage -> print carbage
      carbage -> print carbage
    carbage -> print carbage

shouldAcceptLink atl lh = False
