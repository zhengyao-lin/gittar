-- storage/repository specific implementations

{-# LANGUAGE FlexibleInstances #-}

module Git.Standard where

import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Lazy as BS

import Control.Monad
import Control.Monad.State

import System.Directory
import System.FilePath

import Git.Repo
import Git.Object
import Git.Utils
import Git.Standard.Config
import Git.Standard.Consts

data StandardRepo = StandardRepo {
    repoConfig :: StandardConfig,
    rootPath   :: FilePath,
    storeCache :: Map.HashMap ObjectHash (IO Object)
}

type StandardRepoState = StateT StandardRepo IO

instance RepoState StandardRepoState where
    getObject hash =
        fmap (Map.lookup hash . storeCache) get >>= \mobj ->
            case mobj of
                Just io -> fmap Just (liftIO io)
                Nothing -> return Nothing

    addObject obj = do
        let hash = hashObject obj

        writeObject hash obj

        modify $ \s -> s {
            storeCache = Map.insert hash (return obj) (storeCache s)
        }

        return hash

    listObjects = fmap (Map.keys . storeCache) get

openStandardRepo :: FilePath -> IO StandardRepo
openStandardRepo path = do
    exist <- doesDirectoryExist path
    failIf (constErrorDirectoryNotFound path) (not exist)

    config <- readStandardConfig (path </> constGitDirectory </> constConfigFileName)
    config <- case config of
        Right config -> return config
        Left err -> fail err

    -- traverse the object directory to find all objects
    let objects_path = path </> constGitDirectory </> constObjectsDirectory

    indices <- listDirectory objects_path

    objects <- fmap concat $ forM indices $ \index -> do
        suffixes <- listDirectory (objects_path </> index)

        let valid hash = case maybeRead hash :: Maybe ObjectHash of
                Just _ -> True
                Nothing -> False
        
            -- filter out invalid suffixes
            valid_suffixes = filter (valid . (index ++)) suffixes

        -- the object is not immediately read and parsed
        -- it would be stored as an IO action and read later(but would be cached once read)
        forM valid_suffixes $ \suffix -> do
            io_obj <- once $ do
                let path = objects_path </> index </> suffix
                
                compressed <- BS.readFile path

                case zlibDecompress compressed of
                    Just raw -> case parseObject raw of
                        Right obj -> return obj
                        Left err -> fail (constErrorFailedParseObject path err)

                    Nothing -> fail (constErrorFailedDecompressObject path)

            return (read (index ++ suffix), io_obj)

    return StandardRepo {
        repoConfig = config,
        rootPath = path,
        storeCache = Map.fromList objects
    }

getObjectPath :: ObjectHash -> StandardRepoState (FilePath, FilePath)
getObjectPath hash = do
    let index = take constHashIndexLength (show hash)
        fname = drop constHashIndexLength (show hash)

    s <- get

    return (rootPath s </>
            constGitDirectory </>
            constObjectsDirectory </>
            index, fname)

writeObject :: ObjectHash -> Object -> StandardRepoState ()
writeObject hash obj = do
    let encoded = encodeObject obj
        compressed = zlibCompress encoded

    (dir, fname) <- getObjectPath hash

    liftIO $ createDirectoryIfMissing False dir
    liftIO $ BS.writeFile (dir </> fname) compressed
