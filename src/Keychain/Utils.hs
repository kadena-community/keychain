{-# LANGUAGE OverloadedStrings #-}
module Keychain.Utils where

import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Natural
import Text.Read (readMaybe)

tshow :: Show a => a -> Text
tshow = T.pack . show

hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a

baToText :: ByteArrayAccess b => b -> Text
baToText = T.decodeUtf8 . BA.pack . BA.unpack

textTo :: IsString a => Text -> a
textTo = fromString . T.unpack

toB16 :: ByteString -> Text
toB16 = B16.encodeBase16

fromB16 :: Text -> Either Text ByteString
fromB16 txt = B16.decodeBase16 $ T.encodeUtf8 txt

readNatural :: String -> Maybe Natural
readNatural = readMaybe
