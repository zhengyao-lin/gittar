{-# LANGUAGE OverloadedStrings #-}

-- implementation irrelevant constants
module Git.Consts where

import Data.List

import Text.Printf

import Git.Utils

constHashLength = 20 :: Int

constObjectBlobHeader = "blob" :: StrictByteString
constObjectTreeHeader = "tree" :: StrictByteString
constObjectCommitHeader = "commit" :: StrictByteString
constObjectTagHeader = "tag" :: StrictByteString

constCommitHeaderTree = "tree" :: StrictByteString
constCommitHeaderParent = "parent" :: StrictByteString
constCommitHeaderAuthor = "author" :: StrictByteString
constCommitHeaderCommitter = "committer" :: StrictByteString

constTagHeaderObject = "object" :: StrictByteString
constTagHeaderType = "type" :: StrictByteString
constTagHeaderName = "tag" :: StrictByteString
constTagHeaderTagger = "tagger" :: StrictByteString

constErrorNoSuchObjectType header = printf "no such object type %s" header
constErrorWrongTimestamp = "wrong timestamp format"
constErrorBytesNotConsumed bytes = printf "%d byte(s) left unparsed" bytes
constErrorParseError pos ctx err = printf "parse error: position %d: unparsed with context: [%s]: %s" pos (intercalate "->" ctx) err
constErrorParseHashText = "failed to parse hash text"
