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
import Vindicat

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

$( derive makeArbitrary ''PublicKey )
$( derive makeArbitrary ''Mac )
$( derive makeArbitrary ''EthernetFrame )
$( derive makeArbitrary ''PubKey )
$( derive makeArbitrary ''DeviceProperty )
$( derive makeArbitrary ''LinkProperty )

instance Arbitrary Signature where
  arbitrary = NaclSignature <$> pack <$> replicateM 64 arbitrary

prop_serialize_pubk :: PubKey -> Bool
prop_serialize_pubk pubk =           pubk == UnknownPubKey ||
                               Right pubk == (decode . encode) pubk

prop_serialize_devprop :: DeviceProperty -> Bool
prop_serialize_devprop prop =        prop == UnknownDeviceProperty ||
                               Right prop == (decode . encode) prop

prop_serialize_signature :: Signature -> Bool
prop_serialize_signature sig =        sig == UnknownSignature ||
                                Right sig == (decode . encode) sig

k_prop_sign_verify_nacl :: KeyPair -> ByteString -> Bool
k_prop_sign_verify_nacl (pk,sk) msg
 = True == (verify msg (NaclKey pk) $ naclSign sk msg)

k_prop_sign_verify_bogus_nacl (pk,_) sig msg = False == (verify msg (NaclKey pk) sig)

k_prop_serialize_device :: KeyPair -> Mac -> ByteString -> Bool
k_prop_serialize_device kp mac nick = (Right dev) == ((decode . encode) dev)
    where
    dev = mkDevice kp mac nick

k_prop_serialize_linkhalf :: KeyPair -> PubKey -> Bool
k_prop_serialize_linkhalf kp otherpub
 = otherpub == UnknownPubKey || Right lh == (decode . encode) lh
    where lh = mkLinkHalf kp otherpub []

kk_prop_serialize_link kp1@(pk1,_) kp2@(pk2,_) = Right l == (decode . encode) l
    where
    l = acceptLink kp2 lh
    lh = mkLinkHalf kp1 (NaclKey pk2) []

prop_sign_verify_nacl       = k_prop_sign_verify_nacl       (unsafePerformIO createKeypair)
prop_sign_verify_bogus_nacl = k_prop_sign_verify_bogus_nacl (unsafePerformIO createKeypair)
prop_serialize_device       = k_prop_serialize_device       (unsafePerformIO createKeypair)
prop_serialize_linkhalf     = k_prop_serialize_linkhalf     (unsafePerformIO createKeypair)
prop_serialize_link         = kk_prop_serialize_link        (unsafePerformIO createKeypair) (unsafePerformIO createKeypair)


main = $( quickCheckAll )
