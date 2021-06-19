{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Keychain.Keygen
import Keychain.Utils
import Options.Applicative
import Lens.Micro
import Lens.Micro.TH

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map

data KeychainCommand = KeychainCommand
  { _keychainCommand_subCommand :: KeychainSubCommand
  }
  deriving (Eq, Show)

data KeychainSubCommand =
    KeychainSubCommand_GenSeedPhrase
  -- TODO: MAKE INDEX OPTION, DEFAULT TO 0
  | KeychainSubCommand_GenKeypair Text KeyIndex
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
  KeychainSubCommand_GenKeypair phrase idx -> do
    let root = mnemonicToRoot $ T.words phrase
        (xPrv, pub) = generateCryptoPairFromRoot root "" idx
    T.putStrLn $ T.unlines $
     [ "Index: " <> tshow idx
     , "Public Key: " <> pubKeyToText pub
     , "Encrypted private key: " <> encryptedPrivateKeyToText xPrv
     ]


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
       progDesc "Generate a keypair from a mnemonic phrase" )
  )

generatePhraseCmd :: Parser KeychainSubCommand
generatePhraseCmd = pure $ KeychainSubCommand_GenSeedPhrase

generateKeyPairFromPhrase :: Parser KeychainSubCommand
generateKeyPairFromPhrase= KeychainSubCommand_GenKeypair
  <$> (strArgument $ metavar "MNEMONIC-PHRASE" )
  <*> (argument keyIndexReader $ metavar "INDEX" )
  where
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)

main :: IO ()
main = do
  runKeychain =<< execParser keychainOpts
  pure ()
