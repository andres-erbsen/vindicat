module Network.Pcap.SendReceive (
    PcapHandleSR
  , CallbackSRBS
  , sendReceiveLoopBS
  , sendPacketSRBS
) where

import Data.ByteString (ByteString)
import Network.Pcap
import Control.Concurrent
import Control.Concurrent.Chan

data PcapHandleSR = PcapHandleSR PcapHandle (Chan ByteString) ThreadId
-- TODO: manually write instances for Device instead of this hack
instance Ord PcapHandleSR where compare _ _ = EQ
instance Eq  PcapHandleSR where (==) _ _ = False
instance Show PcapHandleSR where show _ = "[someiface]"

type CallbackSRBS = PcapHandleSR -> PktHdr -> ByteString -> IO ()

launch2 :: (a -> b -> IO r) -> a -> b -> IO ()
launch2 f = \a b -> forkIO (f a b >> return ()) >> return ()

-- launch3 :: (a -> b -> c -> IO r) -> a -> b -> c -> IO ()
-- launch3 f = \a b c -> forkIO (f a b c >> return ()) >> return ()

sendReceiveLoopBS :: CallbackSRBS -> PcapHandle -> IO PcapHandleSR
sendReceiveLoopBS callback iface = do
  setNonBlock iface True -- TODO: setNonBlock?
  sendChan  <- newChan
  tid <- forkIO $ do -- the actual loop
    tid <- myThreadId
    let myHandle = PcapHandleSR iface sendChan tid
    dispatchBS iface (-1) $ launch2 $ callback myHandle
    sendPacketBS iface =<< readChan sendChan
    yield -- TODO: yield?
  return $ PcapHandleSR iface sendChan tid

sendPacketSRBS :: PcapHandleSR  -- ^ send-receive loop handle
               -> ByteString    -- ^ packet data (including link-level header)
               -> IO ()
sendPacketSRBS (PcapHandleSR _ sendChan _) = writeChan sendChan 
