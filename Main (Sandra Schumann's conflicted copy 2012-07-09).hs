import qualified Data.ByteString.Char8 as B
import Data.Serialize
import Data.Ethernet

import Foreign.Marshal.Error (void)
import Control.Monad  hiding (void)
import Control.Concurrent
import Network.Pcap

main = listenForPackets "lo" "0xFFFF"

listenForPackets devName ethType = do
    dev <- openLive devName (read ethType) False 0
    setFilter dev ("ether proto " ++ ethType) True 0
    setNonBlock dev True
    forever $ dispatchBS dev (-1) receive >> yield

receive :: CallbackBS -- PktHdr -> ByteString -> IO ()
receive _ packet = void . forkIO $ do
    print (decode packet :: Either String EthernetFrame)

