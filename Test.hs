{-# LANGUAGE TemplateHaskell #-}

import Data.DeriveTH
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen

import Control.Monad
import Control.Applicative
import System.IO.Unsafe

import Data.Serialize
import qualified Data.ByteString as B
import Data.ByteString (ByteString,pack)
import Crypto.NaCl.Sign hiding (verify)
import Crypto.NaCl.Key

import Control.Concurrent.STM.Graph
import Data.Ethernet
import Data.Time.TAI64
import Vindicat

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

instance Arbitrary Signature where
  arbitrary = NaClSignature <$> pack <$> replicateM 64 arbitrary

instance Arbitrary PublicKey where
  arbitrary = PublicKey <$> pack <$> replicateM 32 arbitrary

$( derive makeArbitrary ''Mac )
$( derive makeArbitrary ''EthernetFrame )
$( derive makeArbitrary ''PubKey )
$( derive makeArbitrary ''DeviceProperty )
$( derive makeArbitrary ''Medium )
$( derive makeArbitrary ''LinkProperty )
$( derive makeArbitrary ''TAI64 )


prop_serialize_pubk :: PubKey -> Bool
prop_serialize_pubk pubk = Right pubk == (decode . encode) pubk

prop_serialize_devprop :: DeviceProperty -> Bool
prop_serialize_devprop prop = Right prop == (decode . encode) prop

prop_serialize_signature :: Signature -> Bool
prop_serialize_signature sig = Right sig == (decode . encode) sig

k_prop_sign_verify_nacl :: KeyPair -> ByteString -> Bool
k_prop_sign_verify_nacl (pk,sk) msg
 = True == (verify msg (NaClKey pk) $ naclSign sk msg)

k_prop_sign_verify_bogus_nacl (pk,_) sig msg = False == (verify msg (NaClKey pk) sig)

k_prop_serialize_device :: KeyPair -> ByteString -> Bool
k_prop_serialize_device kp nick = (Right dev) == (decode . encode $ dev)
    where
    dev = mkDevice kp nick

k_prop_serialize_linkrq :: KeyPair -> [LinkProperty] -> ByteString -> Bool
k_prop_serialize_linkrq kp props nick = Right lrq == (decode . encode) lrq
    where lrq = mkLinkRq kp dev dev props
          dev = mkDevice kp nick

kk_prop_serialize_link kp1 kp2 nick1 nick2 props = Right l ==(decode . encode) l
    where
    l = acceptLink kp2 lh
    lh = mkLinkRq kp1 dev1 dev2 props
    dev1 = mkDevice kp1 nick1
    dev2 = mkDevice kp2 nick2

prop_sign_verify_nacl       = k_prop_sign_verify_nacl       (unsafePerformIO createKeypair)
prop_sign_verify_bogus_nacl = k_prop_sign_verify_bogus_nacl (unsafePerformIO createKeypair)
prop_serialize_device       = k_prop_serialize_device       (unsafePerformIO createKeypair)
prop_serialize_linkrq       = k_prop_serialize_linkrq       (unsafePerformIO createKeypair)
prop_serialize_link         = kk_prop_serialize_link        (unsafePerformIO createKeypair) (unsafePerformIO createKeypair)

main = $( quickCheckAll )
