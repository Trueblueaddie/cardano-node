{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw binary serialisation
--
module Cardano.Api.SerialiseRaw
  ( RawBytesHexError(..)
  , SerialiseAsRawBytes(..)
  , serialiseToRawBytesHex
  , deserialiseFromRawBytesHex
  , serialiseToRawBytesHexText
  ) where

import           Cardano.Prelude
import           Prelude (String)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as Text

import           Cardano.Api.Error (Error, displayError)
import           Cardano.Api.HasTypeProxy

class HasTypeProxy a => SerialiseAsRawBytes a where

  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Maybe a

serialiseToRawBytesHex :: SerialiseAsRawBytes a => a -> ByteString
serialiseToRawBytesHex = Base16.encode . serialiseToRawBytes

serialiseToRawBytesHexText :: SerialiseAsRawBytes a => a -> Text
serialiseToRawBytesHexText = Text.decodeUtf8 . serialiseToRawBytesHex

-- | The errors that the pure 'SerialiseAsRawBytes' parsing\/decoding functions can return.
data RawBytesHexError
  = RawBytesHexErrorBase16DecodeFail
      ByteString -- ^ original input
      String -- ^ error message
  | RawBytesHexErrorRawBytesDecodeFail
      ByteString -- ^ original input
  deriving (Show)

instance Error RawBytesHexError where
  displayError = \case
    RawBytesHexErrorBase16DecodeFail input message ->
      "Expected Base16-encoded bytestring, but got " ++ show input ++ "; "
      ++ message
    RawBytesHexErrorRawBytesDecodeFail input ->
      "Failed to deserialise " ++ show input
      -- TODO(2022-01-26, cblp) show expected output type

deserialiseFromRawBytesHex
  :: SerialiseAsRawBytes a
  => AsType a -> ByteString -> Either RawBytesHexError a
deserialiseFromRawBytesHex proxy hex = do
  raw <- first (RawBytesHexErrorBase16DecodeFail hex) $ Base16.decode hex
  maybe (Left $ RawBytesHexErrorRawBytesDecodeFail hex) Right $
    deserialiseFromRawBytes proxy raw
