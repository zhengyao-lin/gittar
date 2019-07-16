module Git.Utils where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BSU

import qualified Control.Monad.Fail as Fail
import qualified Control.Exception as Exc
import Control.Concurrent.MVar

import System.IO.Unsafe

import qualified Codec.Compression.Zlib as Z

failIf :: Fail.MonadFail m => String -> Bool -> m ()
failIf msg cond =
    if cond then Fail.fail msg
    else return ()

toLazyByteString :: SBS.ByteString -> BS.ByteString
toLazyByteString = BS.fromStrict

toStrictByteString :: BS.ByteString -> SBS.ByteString
toStrictByteString = BS.toStrict

fromString :: String -> BS.ByteString
fromString = BSU.fromString

toString :: BS.ByteString -> String
toString = BSU.toString

zlibCompress :: BS.ByteString -> BS.ByteString
zlibCompress = Z.compress

zlibDecompress :: BS.ByteString -> Maybe BS.ByteString
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
