module Git.Utils where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BSU

import qualified Control.Monad.Fail as Fail
import qualified Control.Exception as Exc

import System.IO.Unsafe

import qualified Codec.Compression.Zlib as Z

failIf :: Fail.MonadFail m => String -> Bool -> m ()
failIf msg cond =
    if cond then Fail.fail msg
    else return ()

toLazyByteString :: BS.ByteString -> LBS.ByteString
toLazyByteString = LBS.fromStrict

toStrictByteString :: LBS.ByteString -> BS.ByteString
toStrictByteString = LBS.toStrict

fromString :: String -> BS.ByteString
fromString = BSU.fromString

zlibCompress :: BS.ByteString -> BS.ByteString
zlibCompress = toStrictByteString . Z.compress . toLazyByteString

zlibDecompress :: BS.ByteString -> Maybe BS.ByteString
zlibDecompress = unsafeCleanup . toStrictByteString . Z.decompress . toLazyByteString

-- nasty solution from https://stackoverflow.com/questions/4243117/how-to-catch-and-ignore-a-call-to-the-error-function
{-# NOINLINE unsafeCleanup #-}
unsafeCleanup :: a -> Maybe a
unsafeCleanup x =
    unsafePerformIO io
    where
        io = Exc.catch (seq x return (Just x)) handler

        handler :: Exc.ErrorCall -> IO (Maybe a)
        handler exc = return Nothing
