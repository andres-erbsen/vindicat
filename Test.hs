{-# LANGUAGE TemplateHaskell #-}

import Data.DeriveTH
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen

import Control.Monad
import Control.Applicative
import System.IO.Unsafe

import Data.Serialize
import Data.ByteString
import Crypto.NaCl.Sign hiding (verify)
import Crypto.NaCl.Key

import Data.Ethernet
import Vindicat

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

$( derive makeArbitrary ''PublicKey )
$( derive makeArbitrary ''Mac )
$( derive makeArbitrary ''EthernetFrame )
$( derive makeArbitrary ''PubKey )
$( derive makeArbitrary ''DeviceProperty )
$( derive makeArbitrary ''Device )

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

prop_sign_verify_nacl :: ByteString -> Bool
prop_sign_verify_nacl msg = True == (verify msg pk $ naclSign sk msg)
  where
    pk = NaclKey pk'
    (pk',sk) = unsafePerformIO createKeypair

prop_sign_verify_bogus_nacl sig msg = False == (verify msg pk sig)
  where
    pk = NaclKey $ fst $ unsafePerformIO createKeypair

prop_serialize_device :: Mac -> ByteString -> Bool
prop_serialize_device mac nick = (Right dev) == ((decode . encode) dev)
    where
    dev = mkDevice kp mac nick
    kp = unsafePerformIO createKeypair

main = $( quickCheckAll )
