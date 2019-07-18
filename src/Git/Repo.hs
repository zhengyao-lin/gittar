module Git.Repo where

import Control.Monad

import Git.Object

-- general model of a repo state
class Monad m => RepoState m where
    -- synchronize changes
    -- sync :: m ()

    getObject :: ObjectHash -> m (Maybe Object)
    addObject :: Object -> m ObjectHash
    listObjects :: m [ObjectHash]
