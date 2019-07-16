module Git.Standard.Consts where

import Text.Printf

constGitDirectory = ".git"
constHashIndexLength = 2 :: Int -- in number of hex characters

constObjectsDirectory = "objects"

constConfigFileName = "config"
constConfigCoreSection = "core"
constConfigCoreSectionVersion = "repositoryformatversion"

constErrorDirectoryNotFound path = printf "directory not found: %s" path
constErrorFileNotFound path = printf "file not found: %s" path
constErrorFailedParseObject path err = printf "failed to parse object %s: %s" path err
constErrorFailedDecompressObject path = printf "failed to decompress object %s" path
