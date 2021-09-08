{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Keychain.Keygen
import Keychain.KeyPair
import Keychain.Utils
import Options.Applicative
import Lens.Micro
import Lens.Micro.TH

import qualified Crypto.Hash as Crypto
import Data.Aeson (Value(..))
import Data.Bifunctor
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Data.ByteString.Base64
import qualified Data.ByteString.Base64.URL as B64Url
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import qualified Data.YAML as Y
import qualified Data.YAML.Aeson as YA
import qualified Data.YAML.Event as Y
import qualified Data.YAML.Schema as Y
import qualified Data.YAML.Token as Y
import Data.Word
import Lens.Micro.Aeson
import System.IO

data KeychainCommand = KeychainCommand
  { _keychainCommand_subCommand :: KeychainSubCommand
  -- , _keychainCommand_keyStore :: FilePath
  }
  deriving (Eq, Show)

type KeyFile = FilePath

data Encoding = Raw | B16 | B64 | B64Url | Yaml
  deriving (Eq,Ord,Show,Read)

encodingToText :: Encoding -> Text
encodingToText = \case
  Raw -> "raw"
  B16 -> "b16"
  B64 -> "b64"
  B64Url -> "b64url"
  Yaml -> "yaml"

textToEncoding :: Text -> Maybe Encoding
textToEncoding = \case
  "raw" -> Just Raw
  "b16" -> Just B16
  "b64" -> Just B64
  "b64url" -> Just B64Url
  "yaml" -> Just Yaml
  _ -> Nothing

genericDecode :: Encoding -> ByteString -> Either Text ByteString
genericDecode Raw = Right
genericDecode B16 = decodeBase16
genericDecode B64 = decodeBase64
genericDecode B64Url = B64Url.decodeBase64
genericDecode Yaml = decodeYamlBS -- We don't actually use the result of this case

decodeYamlBS :: ByteString -> Either Text ByteString
decodeYamlBS bs = do
  v :: Value <- first (T.pack . snd) $ YA.decode1Strict bs
  let mhash = hush . B64Url.decodeBase64 . encodeUtf8 =<< (v ^? key "hash" . _String)
      mcmd = encodeUtf8 <$> (v ^? key "cmd" . _String)
      calcHash = BA.convert . Crypto.hashWith Crypto.Blake2b_256
  case (mhash, mcmd) of
    (Nothing, Nothing) -> Left "YAML must contain a key 'hash' and/or 'cmd'"
    (Just hash, Nothing) -> Right hash
    (Nothing, Just cmd) -> Right $ calcHash cmd
    (Just hash, Just cmd) ->
      if calcHash cmd == hash
        then Right hash
        else Left $ T.unlines
               [ "DANGER!!! The hash does not match the command!  Someone may be trying to get you to sign something malicious!"
               , "If you are sure you want to proceed you should delete either the hash or the cmd from your YAML."
               , "PROCEED WITH GREAT CAUTION!!!"
               ]

readAsEncoding :: Encoding -> IO ByteString
readAsEncoding enc = do
  bs <- B.hGetContents stdin
  let trimmer :: Word8 -> Bool
      trimmer w = case enc of
                    Raw -> False
                    _ -> isSpace $ chr (fromIntegral w)
  pure $ B.dropWhile trimmer $ B.dropWhileEnd trimmer bs

data KeychainSubCommand =
    KeychainSubCommand_GenSeedPhrase
  -- TODO: MAKE INDEX OPTION, DEFAULT TO 0
  | KeychainSubCommand_GetKeypair KeyFile KeyIndex
  | KeychainSubCommand_Sign KeyFile (Maybe KeyIndex) Encoding
  | KeychainSubCommand_ValidateYaml FilePath
  | KeychainSubCommand_Verify PublicKey Signature Encoding
  | KeychainSubCommand_ListKeys KeyFile KeyIndex
  deriving (Eq, Show)

makeLenses ''KeychainSubCommand
makeLenses ''KeychainCommand

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
    mkeymaterial <- readKeyMaterial keyfile index
    case mkeymaterial of
      Nothing -> putStrLn $ "Error reading key material from " <> keyfile
      Just material -> do
        T.putStrLn $ "Decoding with " <> encodingToText enc
        rawbs <- readAsEncoding enc
        let ebs = genericDecode enc rawbs
        case ebs of
          Left e -> do
            T.putStrLn $ "Error decoding stdin as " <> encodingToText enc <> ":\n" <> e
          Right msg -> do
            let pub = getMaterialPublic material
                sig = signWithMaterial material msg
            case enc of
              Yaml -> do
                let res = do
                      v :: Value <- first  (T.pack . snd) $ YA.decode1Strict rawbs
                      pure (v & key "sigs" . key pub .~ String (toB16 sig))
                    encScalar s@(Y.SStr t) = case T.find (== '"') t of
                      Just _ -> Right (Y.untagged, Y.SingleQuoted, t)
                      Nothing -> Y.schemaEncoderScalar Y.coreSchemaEncoder s
                    encScalar s = Y.schemaEncoderScalar Y.coreSchemaEncoder s
                    senc = Y.setScalarStyle encScalar Y.coreSchemaEncoder
                case res of
                  Right v -> LB.putStrLn $ YA.encodeValue' senc Y.UTF8 [v]
                  Left e -> T.putStrLn e
              _ -> T.putStrLn $ pub <> ": " <> toB16 sig
  KeychainSubCommand_ValidateYaml yamlfile -> do
    bs <- B.readFile yamlfile
    case decodeYamlBS bs of
      Left e -> do
        putStrLn $ "There was a problem with tx yaml file " <> yamlfile
        T.putStrLn e
      Right _ -> putStrLn "Your tx yaml passes basic consistency checks"
  KeychainSubCommand_Verify pubkey signature enc -> do
    rawbs <- readAsEncoding enc
    let emsg = genericDecode enc rawbs
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
  <> command "sign" (info signWithKeyMaterial $ progDesc "Sign with a mnemonic phrase or standalone key pair")
  <> command "validate-yaml" (info validateParser $ progDesc "Validate the hash of a yaml transaction")
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
                        (short 'e' <> long "encoding" <> value Yaml <> help "Message encoding (raw, b16, b64, b64url, or yaml (default: yaml))")

signWithKeyMaterial :: Parser KeychainSubCommand
signWithKeyMaterial = KeychainSubCommand_Sign
  <$> (argument str $ metavar "KEY-FILE" )
  <*> optional (argument keyIndexReader $ metavar "INDEX" )
  <*> encodingOption
  where
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)

validateParser :: Parser KeychainSubCommand
validateParser = KeychainSubCommand_ValidateYaml
  <$> (argument str $ metavar "YAML-FILE" )

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
