{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant id" #-}
module Library.Monads.Parsers.FilePack where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BC
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word32)
import System.Posix.Types (FileMode)

data FileData a = FileData
    { fileName :: FilePath
    , fileSize :: Word32
    , filePermissions :: FileMode
    , fileData :: a
    }
    deriving (Eq, Show)

class Encode a where
    encode :: a -> Data.ByteString.ByteString

instance Encode Data.ByteString.ByteString where
    encode = id

class Decode a where
    decode :: Data.ByteString.ByteString -> Either String a

instance Decode Data.ByteString.ByteString where
    decode = Right . id

instance Encode Text where
    encode = encodeUtf8

instance Decode Text where
    decode = Right . decodeUtf8

instance Encode String where
    encode = BC.pack

instance Decode String where
    decode = Right . BC.unpack

