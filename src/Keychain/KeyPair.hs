module Keychain.KeyPair where

import qualified Cardano.Crypto.Wallet as Crypto
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString as BS
-- import qualified Data.Text as T
import Keychain.Utils

newtype PublicKey = PublicKey ByteString
  deriving (Show, Eq)

newtype EncryptedPrivateKey =
  EncryptedPrivateKey { unEncryptePrivateKey :: Crypto.XPrv }

newtype Signature = Signature Crypto.XSignature
  deriving (Show, Eq)

sigToText :: Signature -> Text
sigToText (Signature sig) = toB16 $ Crypto.unXSignature sig

toSignature :: ByteString -> Either String Signature
toSignature = fmap Signature . Crypto.xsignature

pubKeyToText :: PublicKey -> Text
pubKeyToText (PublicKey pub) = toB16 pub

--TODO -- YUCK
toPubKey :: Text -> Either Text PublicKey
toPubKey txt = do
  bs <- fromB16 txt
  case BS.length bs /= 64 of
    False -> Left "PublicKey should be 64 hex characters"
    True -> pure $ PublicKey bs

encryptedPrivateKeyToText :: EncryptedPrivateKey -> Text
encryptedPrivateKeyToText (EncryptedPrivateKey xprv) = toB16 $ Crypto.unXPrv xprv

sign :: EncryptedPrivateKey -> ByteString -> Signature 
sign (EncryptedPrivateKey xprv) msg =
  Signature $ Crypto.sign @ByteString "" xprv msg

verify :: PublicKey -> Signature -> ByteString -> Bool
verify (PublicKey pub) (Signature sig) msg = Crypto.verify xpub msg sig
  where
    dummyChainCode = BS.replicate 32 minBound
    Right xpub = Crypto.xpub $ pub <> dummyChainCode

-- newtype Ed25519CryptoT m a =
--   Ed25519CryptoT { unEd25519CryptoT :: ReaderT () m a }

-- class Monad m => MonadSign m where
--   sign :: ByteString -> PublicKey -> m Signature
--   -- verify :: ByteString -> PublicKey -> Signature -> m Bool
