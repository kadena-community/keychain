{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Keychain.Keygen
import Keychain.KeyPair
import Keychain.Utils
import Options.Applicative
import Lens.Micro
import Lens.Micro.TH

import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Data.ByteString.Base64
import qualified Data.ByteString.Base64.URL as B64Url
import qualified Data.ByteString as B
import Data.Char
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import System.IO

data KeychainCommand = KeychainCommand
  { _keychainCommand_subCommand :: KeychainSubCommand
  -- , _keychainCommand_keyStore :: FilePath
  }
  deriving (Eq, Show)

type KeyFile = FilePath

data Encoding = Raw | B16 | B64 | B64Url
  deriving (Eq,Ord,Show,Read)

encodingToText :: Encoding -> Text
encodingToText = \case
  Raw -> "raw"
  B16 -> "b16"
  B64 -> "b64"
  B64Url -> "b64url"

textToEncoding :: Text -> Maybe Encoding
textToEncoding = \case
  "raw" -> Just Raw
  "b16" -> Just B16
  "b64" -> Just B64
  "b64url" -> Just B64Url
  _ -> Nothing

genericDecode :: Encoding -> ByteString -> Either Text ByteString
genericDecode Raw = Right
genericDecode B16 = decodeBase16
genericDecode B64 = decodeBase64
genericDecode B64Url = B64Url.decodeBase64

readAsEncoding :: Encoding -> IO (Either Text ByteString)
readAsEncoding enc = do
  bs <- B.hGetContents stdin
  let trimmer :: Word8 -> Bool
      trimmer w = case enc of
                    Raw -> False
                    _ -> isSpace $ chr (fromIntegral w)
  pure $ genericDecode enc $ B.dropWhile trimmer $ B.dropWhileEnd trimmer bs

data KeychainSubCommand =
    KeychainSubCommand_GenSeedPhrase
  -- TODO: MAKE INDEX OPTION, DEFAULT TO 0
  | KeychainSubCommand_GetKeypair KeyFile KeyIndex
  | KeychainSubCommand_Sign KeyFile KeyIndex Encoding
  | KeychainSubCommand_Verify PublicKey Signature Encoding
  | KeychainSubCommand_ListKeys KeyFile KeyIndex
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
  KeychainSubCommand_GetKeypair keyfile idx -> do
    Just phrase <- readPhraseFromFile keyfile
    let root = mnemonicToRoot phrase
        (xPrv, pub) = generateCryptoPairFromRoot root "" idx
    T.putStrLn $ T.unlines $
     [ "Index: " <> tshow idx
     , "Public Key: " <> pubKeyToText pub
     , "Encrypted private key: " <> encryptedPrivateKeyToText xPrv
     ]
  KeychainSubCommand_Sign keyfile index enc -> do
    mphrase <- readPhraseFromFile keyfile
    case mphrase of
      Nothing -> putStrLn $ "Error reading seed phrase from " <> keyfile
      Just phrase -> do
        T.putStrLn $ "Decoding with " <> encodingToText enc
        ebs <- readAsEncoding enc
        case ebs of
          Left e -> do
            T.putStrLn $ "Error decoding stdin as " <> encodingToText enc <> ": " <> e
          Right msg -> do
            let (xprv, pub) = genPairFromPhrase phrase index
                sig = sign xprv msg
            T.putStrLn $ "PublicKey: " <> pubKeyToText pub
            T.putStrLn $ "Signature: " <> sigToText sig
            T.putStrLn $ pubKeyToText pub <> ": " <> sigToText sig
  KeychainSubCommand_Verify pubkey signature enc -> do
    emsg <- readAsEncoding enc
    case emsg of
      Left e ->
        T.putStrLn $ "Error decoding stdin as " <> encodingToText enc <> ": " <> e
      Right msg ->
        T.putStrLn $ "Verify: " <> (tshow $ verify pubkey signature msg)
  KeychainSubCommand_ListKeys keyfile index -> do
    Just phrase <- readPhraseFromFile keyfile
    let root = mnemonicToRoot phrase
        getAndShow n = tshow (unKeyIndex n) <> ": " <> pubKeyToText (snd $ generateCryptoPairFromRoot root "" n)
    mapM_ (T.putStrLn . getAndShow) [0..index]


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
  (  command "gen" (info generatePhraseCmd $ progDesc "Generate a mnemonic phrase" )
  <> command "key" (info generateKeyPairFromPhrase $ progDesc "Show a keypair from a mnemonic phrase")
  <> command "sign" (info signWithPhrase $ progDesc "Sign")
  <> command "verify" (info verifySignature $ progDesc "Verify")
  <> command "list" (info listKeys $ progDesc "List keys")
  )

generatePhraseCmd :: Parser KeychainSubCommand
generatePhraseCmd = pure $ KeychainSubCommand_GenSeedPhrase

generateKeyPairFromPhrase :: Parser KeychainSubCommand
generateKeyPairFromPhrase= KeychainSubCommand_GetKeypair
  <$> (argument str $ metavar "KEY-FILE" )
  <*> (argument keyIndexReader $ metavar "INDEX" )
  where
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)

encodingOption :: Parser Encoding
encodingOption = option (maybeReader $ textToEncoding . T.pack)
                        (short 'e' <> long "encoding" <> value B64Url <> help "Message encoding (raw, b16, b64, or b64url)")

signWithPhrase :: Parser KeychainSubCommand
signWithPhrase = KeychainSubCommand_Sign
  <$> (argument str $ metavar "KEY-FILE" )
  <*> (argument keyIndexReader $ metavar "INDEX" )
  <*> encodingOption
  where
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)

verifySignature :: Parser KeychainSubCommand
verifySignature = KeychainSubCommand_Verify
  <$> (argument pubKeyReader $ metavar "PUB-KEY" )
  <*> (argument signatureReader $ metavar "SIGNATURE" )
  <*> encodingOption
  where
    pubKeyReader = maybeReader (hush . toPubKey . T.pack)
    signatureReader = eitherReader (
      (either (Left . T.unpack) toSignature)
      . fromB16
      . T.pack )

listKeys :: Parser KeychainSubCommand
listKeys = KeychainSubCommand_ListKeys
  <$> (argument str $ metavar "KEY-FILE" )
  <*> (argument keyIndexReader $ metavar "INDEX" )
  where
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)


main :: IO ()
main = do
  runKeychain =<< execParser keychainOpts
  pure ()
