-- storage/repository specific implementations

{-# LANGUAGE FlexibleInstances #-}

module Git.Standard where

import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString as BS

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
    storeCache :: Map.HashMap ObjectHash Object
}

type StandardRepoState = StateT StandardRepo IO

instance RepoState StandardRepoState where
    getObject hash = do
        fmap (Map.lookup hash . storeCache) get

    addObject obj = do
        let hash = hashObject obj

        writeObject hash obj

        modify $ \s -> s {
            storeCache = Map.insert hash obj (storeCache s)
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

    return StandardRepo {
        repoConfig = config,
        rootPath = path,
        storeCache = Map.empty -- might not be the full representation of the object store
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
