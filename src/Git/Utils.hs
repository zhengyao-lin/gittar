module Git.Utils where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB

import qualified Data.ByteString.UTF8 as BSU

import qualified Control.Monad.Fail as Fail
import qualified Control.Exception as Exc
import Control.Concurrent.MVar

import System.IO.Unsafe

import qualified Codec.Compression.Zlib as Z

type StrictByteString = SBS.ByteString
type LazyByteString = LBS.ByteString

failIf :: Fail.MonadFail m => String -> Bool -> m ()
failIf msg cond =
    if cond then Fail.fail msg
    else return ()

toLazyByteString :: StrictByteString -> LazyByteString
toLazyByteString = LBS.fromStrict

toStrictByteString :: LazyByteString-> StrictByteString
toStrictByteString = LBS.toStrict

zlibCompress :: LazyByteString -> LazyByteString
zlibCompress = Z.compress

zlibDecompress :: LazyByteString -> Maybe LazyByteString
zlibDecompress = unsafeCleanup . Z.decompress

-- nasty solution from https://stackoverflow.com/questions/4243117/how-to-catch-and-ignore-a-call-to-the-error-function
{-# NOINLINE unsafeCleanup #-}
unsafeCleanup :: a -> Maybe a
unsafeCleanup x =
    unsafePerformIO io
    where
        io = Exc.catch (seq x return (Just x)) handler

        handler :: Exc.ErrorCall -> IO (Maybe a)
        handler exc = return Nothing

-- carry out an IO action once and cache the result
once :: IO a -> IO (IO a)
once action = do
    mvar <- newMVar Nothing -- cache of the result

    return $ modifyMVar mvar $ \m ->
        case m of
            Just a -> return (Just a, a)
            Nothing -> do
                a <- action
                return (Just a, a)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
    where listToMaybe (a:_) = Just a
          listToMaybe _ = Nothing

toChar :: Enum a => a -> Char
toChar = toEnum . fromEnum

-- stringToUTF8 :: String -> BS.ByteString
-- stringToUTF8 = BSU.fromString

-- UTF8toString :: StrictByteString -> String
-- UTF8toString = BSU.toString

encodeUTF8 :: String -> BSB.Builder
encodeUTF8 = BSB.byteString . BSU.fromString

decodeUTF8 :: StrictByteString -> String
decodeUTF8 = BSU.toString

-- NOTE: these ones only support UTF-8 strings
showByteString :: Show a => a -> StrictByteString
showByteString = BSU.fromString . show

showByteStringBuilder :: Show a => a -> BSB.Builder
showByteStringBuilder = BSB.byteString . showByteString

readByteString :: Read a => StrictByteString -> a
readByteString = read . BSU.toString
