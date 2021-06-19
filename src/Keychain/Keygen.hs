module Keychain.Keygen
  ( PublicKey(..)
  , pubKeyToText
  , mnemonicToRoot
  , genMnemonic12
  , generateCryptoPairFromRoot
  , mkPhraseMapFromMnemonic
  , KeyIndex(..)
  , EncryptedPrivateKey
  , encryptedPrivateKeyToText
  ) where

import qualified Cardano.Crypto.Wallet as Crypto
import Control.Monad.IO.Class
import qualified Crypto.Encoding.BIP39 as Crypto
import qualified Crypto.Encoding.BIP39.English as Crypto
import qualified Crypto.Random.Entropy
import Data.Bifunctor
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Natural
import Data.Word (Word32)
import Keychain.Utils

mnemonicToRoot :: [Text] -> Crypto.XPrv
mnemonicToRoot phrase = seedToRoot (phraseToSeed phrase) "" -- TODO: Empty passowrd

genMnemonic12 :: MonadIO m => m (Either Text (Crypto.MnemonicSentence 12))
genMnemonic12 = liftIO $ bimap tshow Crypto.entropyToWords . Crypto.toEntropy @128
  -- This size must be a 1/8th the size of the 'toEntropy' size: 128 / 8 = 16
  <$> Crypto.Random.Entropy.getEntropy @ByteString 16

generateCryptoPairFromRoot :: Crypto.XPrv -> Text -> KeyIndex -> (EncryptedPrivateKey, PublicKey)
generateCryptoPairFromRoot root pass i =
  let hardenedIdx = 0x80000000 .|. (fromKeyIndex i)
      xprv = Crypto.deriveXPrv scheme (T.encodeUtf8 pass) root hardenedIdx
  in (EncryptedPrivateKey xprv, PublicKey $ Crypto.xpubPublicKey $ Crypto.toXPub xprv)
  where
    scheme = Crypto.DerivationScheme2

mkPhraseMapFromMnemonic
  :: forall mw.
     Crypto.ValidMnemonicSentence mw
  => Crypto.MnemonicSentence mw
  -> Map.Map WordKey Text
mkPhraseMapFromMnemonic = wordsToPhraseMap . T.words . baToText
  . Crypto.mnemonicSentenceToString @mw Crypto.english

newtype PublicKey = PublicKey ByteString
  deriving (Show, Eq)

newtype EncryptedPrivateKey = EncryptedPrivateKey Crypto.XPrv

pubKeyToText :: PublicKey -> Text
pubKeyToText (PublicKey pub) = toB16 pub

encryptedPrivateKeyToText :: EncryptedPrivateKey -> Text
encryptedPrivateKeyToText (EncryptedPrivateKey xprv) = toB16 $ Crypto.unXPrv xprv

-- TODO: Don't expose constructor; only create with 'mkKeyIndex'
newtype KeyIndex = KeyIndex { unKeyIndex :: Natural }
  deriving (Show, Eq)

fromKeyIndex :: KeyIndex -> Word32
fromKeyIndex = fromIntegral . naturalToInt . unKeyIndex

--------------------------------------------------

-- genMnemonic24 :: MonadIO m => m (Either Text (Crypto.MnemonicSentence 24))
-- genMnemonic24 = liftIO $ bimap tshow Crypto.entropyToWords . Crypto.toEntropy @256
--   -- This size must be a 1/8th the size of the 'toEntropy' size: 256 / 8 = 32
--   <$> Crypto.Random.Entropy.getEntropy @ByteString 32

-- for recovery
phraseToSeed :: [ Text ] -> Crypto.Seed
phraseToSeed lst =
  let phraseMap = wordsToPhraseMap lst
      Right phrase = Crypto.mnemonicPhrase @12 $ textTo <$> Map.elems phraseMap
      Right sentence = Crypto.mnemonicPhraseToMnemonicSentence Crypto.english phrase
  in sentenceToSeed sentence

-- for generation
sentenceToSeed :: Crypto.ValidMnemonicSentence mw => Crypto.MnemonicSentence mw -> Crypto.Seed
sentenceToSeed s = Crypto.sentenceToSeed s Crypto.english ""

-- |Takes a n-sentence crypto seed and a password, and produces an encrypted key that can be
-- unlocked with the password
-- TODO: enter password 2x, to confirm
seedToRoot :: Crypto.Seed -> Text -> Crypto.XPrv
seedToRoot seed password = Crypto.generate seed (T.encodeUtf8 password)

-- | Convenience function for unpacking byte array things into 'Text'
newtype WordKey = WordKey { _unWordKey :: Int }
  deriving (Show, Eq, Ord, Enum)

wordsToPhraseMap :: [Text] -> Map.Map WordKey Text
wordsToPhraseMap = Map.fromList . zip [WordKey 1 ..]
