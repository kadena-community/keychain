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

baToText :: ByteArrayAccess b => b -> Text
baToText = T.decodeUtf8 . BA.pack . BA.unpack

textTo :: IsString a => Text -> a
textTo = fromString . T.unpack

toB16 :: ByteString -> Text
toB16 = T.decodeUtf8 . B16.encode

fromB16 :: Text -> Maybe ByteString
fromB16 txt = case B16.decode $ T.encodeUtf8 txt of
  (res, "") -> Just res
  _ -> Nothing

readNatural :: String -> Maybe Natural
readNatural = readMaybe
