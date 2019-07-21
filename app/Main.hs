{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time
import qualified Data.ByteString.Lazy as LBS

import Control.Monad.State

import Text.Printf

import Git.Object
import Git.Repo
import Git.Utils
import Git.Standard
import Git.Standard.Config

main :: IO ()
main = do
    now <- getZonedTime

    let file = Blob "hey\n"
        file_hash = hashObject file
        
        tree = Tree [
                TreeEntry 100644 "new-file" file_hash,
                TreeEntry 100644 "new-file-same" file_hash
            ]
        tree_hash = hashObject tree

        commit = Commit {
                commitTree = tree_hash,
                commitParent = Nothing,
                commitAuthor = UserStamp "zhengyao lin <nobody@illinois.edu>" now,
                commitIssuer = UserStamp "zhengyao lin <nobody@illinois.edu>" now,
                commitMsg = "fix that"
            }

        encoded = encodeObject commit

    putStrLn (decodeUTF8 $ toStrictByteString $ encoded)
    putStrLn (show $ hashObject commit)

    case parseObject encoded of
        Right obj -> putStrLn (show obj)
        Left err -> putStrLn (show err)

test :: IO ()
test = do
    cont <- LBS.readFile "../dummy/.git/objects/db/7559db3bd70aefdf39d3c70cfc0095ce340a7d"

    let Just decompressed = zlibDecompress cont

    case parseObject decompressed of
        Right obj -> putStrLn (show obj)
        Left obj -> putStrLn (show obj)

    repo <- openStandardRepo "../dummy"

    putStrLn (repoFormatVersion (repoConfig repo))
    
    let operations :: IO [ObjectHash]
        operations = flip evalStateT repo $ do
            -- file <- addObject (Blob "hey\n")

            -- tree <- addObject (Tree [
            --         TreeEntry 100644 "new-file" file,
            --         TreeEntry 100644 "new-file-same" file
            --     ])

            -- now <- liftIO getZonedTime

            -- commit <- addObject (Commit {
            --         commitTree = tree,
            --         commitParent = Nothing,
            --         commitAuthor = UserStamp "zhengyao <nobody@illinois.edu>" now,
            --         commitIssuer = UserStamp "zhengyao <nobody@illinois.edu>" now,
            --         commitMsg = "not me"
            --     })

            -- commit <- addObject (Commit {
            --         commitTree = tree,
            --         commitParent = Just commit,
            --         commitAuthor = UserStamp "rod-lin <zl38@illinois.edu>" now,
            --         commitIssuer = UserStamp "rod-lin <zl38@illinois.edu>" now,
            --         commitMsg = "not you"
            --     })

            Just (Blob file) <- getObject (read "6c5d4031e03408e34ae476c5053ee497a91ac37b")
            
            liftIO $ putStrLn (printf "file size %d" (LBS.length file))

            listObjects

    lists <- operations
    putStrLn (show lists)

    return ()
