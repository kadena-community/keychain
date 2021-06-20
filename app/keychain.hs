{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Keychain.Keygen
import Keychain.KeyPair
import Keychain.Utils
import Options.Applicative
import Lens.Micro
import Lens.Micro.TH

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map

data KeychainCommand = KeychainCommand
  { _keychainCommand_subCommand :: KeychainSubCommand
  -- , _keychainCommand_keyStore :: FilePath
  }
  deriving (Eq, Show)

data KeychainSubCommand =
    KeychainSubCommand_GenSeedPhrase
  -- TODO: MAKE INDEX OPTION, DEFAULT TO 0
  | KeychainSubCommand_GenKeypair MnemonicPhrase KeyIndex
  | KeychainSubCommand_Sign MnemonicPhrase KeyIndex Text
  | KeychainSubCommand_Verify PublicKey Signature Text
  | KeychainSubCommand_ListKeys
  deriving (Eq, Show)

makeLenses ''KeychainSubCommand
makeLenses ''KeychainCommand

genPairFromPhrase :: MnemonicPhrase -> KeyIndex -> (EncryptedPrivateKey, PublicKey) 
genPairFromPhrase phrase idx = 
  generateCryptoPairFromRoot (mnemonicToRoot phrase) "" idx

runKeychain :: KeychainCommand -> IO ()
runKeychain cmd = case cmd ^. keychainCommand_subCommand of
  KeychainSubCommand_GenSeedPhrase -> do
    --TODO: write to file option
    let toPhrase = T.unwords . Map.elems . mkPhraseMapFromMnemonic
    let prettyErr e = "ERROR generating menmonic: " <> tshow e
    res <- either prettyErr toPhrase <$> genMnemonic12
    T.putStrLn res
  KeychainSubCommand_GenKeypair phrase idx -> do
    let root = mnemonicToRoot phrase
        (xPrv, pub) = generateCryptoPairFromRoot root "" idx
    T.putStrLn $ T.unlines $
     [ "Index: " <> tshow idx
     , "Public Key: " <> pubKeyToText pub
     , "Encrypted private key: " <> encryptedPrivateKeyToText xPrv
     ]
  KeychainSubCommand_Sign phrase index msg -> do
    let (xprv, pub) = genPairFromPhrase phrase index
        sig = sign xprv $ T.encodeUtf8 msg
    T.putStrLn $ "PublicKey: " <> pubKeyToText pub
    T.putStrLn $ "Signature: " <> sigToText sig
  KeychainSubCommand_Verify pubkey signature msg ->
    T.putStrLn $ "Verify: " <> (tshow $ verify pubkey signature $ T.encodeUtf8 msg)

keychainOpts :: ParserInfo KeychainCommand
keychainOpts = info (keychainParser <**> helper)
  (  progDesc "A key management system for the Kadena blockchain"
  -- <>
  )

keychainParser :: Parser KeychainCommand
keychainParser = KeychainCommand
  <$> keychainSubCmdParser

keychainSubCmdParser :: Parser KeychainSubCommand
keychainSubCmdParser = hsubparser $
  (  command "generate-phrase" (info generatePhraseCmd $ progDesc "Generate a mnemonic phrase" )
  <> command "keypair-from-phrase" (info generateKeyPairFromPhrase $ 
       progDesc "Generate a keypair from a mnemonic phrase")
  <> command "sign-with-phrase" (info signWithPhrase $ progDesc "Sign")
  <> command "verify-signature" (info verifySignature $ progDesc "Verify")
  )

generatePhraseCmd :: Parser KeychainSubCommand
generatePhraseCmd = pure $ KeychainSubCommand_GenSeedPhrase

generateKeyPairFromPhrase :: Parser KeychainSubCommand
generateKeyPairFromPhrase= KeychainSubCommand_GenKeypair
  <$> (argument mnemonicReader $ metavar "MNEMONIC-PHRASE" )
  <*> (argument keyIndexReader $ metavar "INDEX" )
  where
    mnemonicReader = maybeReader (mkMnemonicPhrase . T.words . T.pack)
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)

signWithPhrase :: Parser KeychainSubCommand
signWithPhrase = KeychainSubCommand_Sign
  <$> (argument mnemonicReader $ metavar "MNEMONIC-PHRASE" )
  <*> (argument keyIndexReader $ metavar "INDEX" )
  <*> (strArgument $ metavar "MESSAGE" )
  where
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)
    mnemonicReader = maybeReader (mkMnemonicPhrase . T.words . T.pack)

verifySignature :: Parser KeychainSubCommand
verifySignature = KeychainSubCommand_Verify
  <$> (argument pubKeyReader $ metavar "PUBLIC KEY" )
  <*> (argument signatureReader $ metavar "SIGNATURE" )
  <*> (strArgument $ metavar "MESSAGE" )
  where
    pubKeyReader = maybeReader (toPubKey . T.pack)
    signatureReader = eitherReader (
      (maybe (Left "Must be b16 encoded") toSignature)
      . fromB16
      . T.pack )
      

main :: IO ()
main = do
  runKeychain =<< execParser keychainOpts
  pure ()
