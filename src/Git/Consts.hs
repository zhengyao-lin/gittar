-- implementation irrelevant constants
module Git.Consts where

import Text.Printf

constHashLength = 20 :: Int

constObjectBlobHeader = "blob"
constObjectTreeHeader = "tree"
constObjectCommitHeader = "commit"

constCommitHeaderTree = "tree"
constCommitHeaderParent = "parent"
constCommitHeaderAuthor = "author"
constCommitHeaderCommitter = "committer"

constErrorNoSuchObjectType header = printf "no such object type %s" header
constErrorWrongTimestamp = "wrong timestamp format"
