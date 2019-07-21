{-# LANGUAGE OverloadedStrings #-}

module Git.Object where

import Data.Time
import Data.List as List
import Data.Monoid
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import Data.Hashable
import qualified Data.Char as Char
import Data.List.Split
import Data.Attoparsec.ByteString.Lazy as P

import Data.Word8

import Debug.Trace

import Crypto.Hash.SHA1 as SHA1

import Control.Applicative

import Numeric

import Git.Utils
import Git.Consts

type TreeEntryMode = Int
data TreeEntry = TreeEntry TreeEntryMode StrictByteString ObjectHash deriving (Show)

data UserStamp = UserStamp StrictByteString ZonedTime deriving (Show)

data ObjectType = BlobType | TreeType | CommitType | TagType deriving (Show)

data Object =
    Blob LazyByteString |
    Tree [TreeEntry] |
    Commit {
        commitTree   :: ObjectHash,
        commitParent :: Maybe ObjectHash,
        commitAuthor :: UserStamp,
        commitIssuer :: UserStamp,
        commitMsg    :: LazyByteString
    } |
    Tag {
        tagObject :: ObjectHash,
        tagType   :: ObjectType,
        tagName   :: StrictByteString,
        tagTagger :: UserStamp,
        tagMsg    :: LazyByteString
    }
    deriving (Show)

newtype ObjectHash = ObjectHash StrictByteString deriving (Eq, Ord)

instance Show ObjectHash where
    show (ObjectHash hash) = concat $ do
        c <- SBS.unpack hash
        let hc = showHex c ""
            len = length hc

        if len < 2 then
            return $ replicate (2 - len) '0' ++ hc
        else
            return hc

instance Read ObjectHash where
    readsPrec _ input =
        if length input == constHashLength * 2 && all Char.isHexDigit input then
            let raw =
                    SBS.pack $ flip map [0,2..constHashLength * 2 - 1] $ \i ->
                        let [(val, _)] = readHex [input !! i, input !! (i + 1)]
                        in fromIntegral val
            in [(ObjectHash raw, "")]
        else []

instance Hashable ObjectHash where
    hashWithSalt salt (ObjectHash hash) = hashWithSalt salt hash

hashObject :: Object -> ObjectHash
hashObject = ObjectHash . SHA1.hashlazy . encodeObject

-- encoders
encodeObjectHash :: ObjectHash -> BSB.Builder
encodeObjectHash (ObjectHash hash) = BSB.byteString hash

encodeTreeEntry :: TreeEntry -> BSB.Builder
encodeTreeEntry (TreeEntry mode name hash) =
    showByteStringBuilder mode <> " " <>
    BSB.byteString name <> "\0" <>
    encodeObjectHash hash

encodeUserStamp :: UserStamp -> BSB.Builder
encodeUserStamp (UserStamp info time) =
    BSB.byteString info <> " " <>
    encodeUTF8 (formatTime defaultTimeLocale "%s" time) <> " " <>
    encodeUTF8 (timeZoneOffsetString (zonedTimeZone time))

encodeObjectType :: ObjectType -> BSB.Builder
encodeObjectType BlobType = BSB.byteString constObjectBlobHeader
encodeObjectType TreeType = BSB.byteString constObjectTreeHeader
encodeObjectType CommitType = BSB.byteString constObjectCommitHeader
encodeObjectType TagType = BSB.byteString constObjectTagHeader

-- general encoding for all object types
encodeObjectTemplate :: StrictByteString -> BSB.Builder -> BSB.Builder
encodeObjectTemplate header cont =
    BSB.byteString header <> " " <> showByteStringBuilder len <> "\0" <> cont
    where len = LBS.length (BSB.toLazyByteString cont)

encodeObject' :: Object -> BSB.Builder
encodeObject' (Blob cont) = encodeObjectTemplate constObjectBlobHeader (BSB.lazyByteString cont)
encodeObject' (Tree entries) = encodeObjectTemplate constObjectTreeHeader cont
    where cont = mconcat (map encodeTreeEntry entries)

encodeObject' Commit {
    commitTree = tree,
    commitParent = mparent,
    commitAuthor = author,
    commitIssuer = committer,
    commitMsg = msg
} =
    encodeObjectTemplate constObjectCommitHeader cont
    where
        cont = headers <> "\n" <> BSB.lazyByteString msg

        parent = case mparent of
            Nothing -> mempty
            Just hash ->
                BSB.byteString constCommitHeaderParent <> " " <> showByteStringBuilder hash <> "\n"
        
        headers =
            BSB.byteString constCommitHeaderTree <> " " <> showByteStringBuilder tree <> "\n" <>
            parent <>
            BSB.byteString constCommitHeaderAuthor <> " " <> encodeUserStamp author <> "\n" <>
            BSB.byteString constCommitHeaderCommitter <> " " <> encodeUserStamp committer <> "\n"

encodeObject' Tag {
    tagObject = obj,
    tagType = ttype,
    tagName = name,
    tagTagger = tagger,
    tagMsg = msg
} =
    mconcat [
        BSB.byteString constTagHeaderObject <> " " <> showByteStringBuilder obj <> "\n",
        BSB.byteString constTagHeaderType <> " " <> encodeObjectType ttype <> "\n",
        BSB.byteString constTagHeaderName <> " " <> BSB.byteString name <> "\n",
        BSB.byteString constTagHeaderTagger <> " " <> encodeUserStamp tagger <> "\n",
        "\n",
        BSB.lazyByteString msg
    ]

encodeObject :: Object -> LazyByteString
encodeObject =  BSB.toLazyByteString . encodeObject'

parseObject :: LazyByteString -> Either String Object
parseObject raw =
    case parse objectP raw of
        Fail rest ctx err ->
            Left (constErrorParseError (LBS.length raw - LBS.length rest) ctx err)

        Done rest obj ->
            if LBS.null rest then
                Right obj
            else
                Left (constErrorBytesNotConsumed (LBS.length rest))

    where
        isNewline = (== toAscii '\n')

        digit = satisfy isDigit <?> "digit"
        hexDigit = satisfy isHexDigit <?> "hex digit"
        space = satisfy isSpace <?> "space"
        letter = satisfy isAlpha <?> "letter"
        newline = satisfy isNewline <?> "newline"

        blobObjectP :: Parser Object
        blobObjectP = fmap Blob takeLazyByteString

        objectHashP :: Parser ObjectHash
        objectHashP = fmap ObjectHash (P.take constHashLength)

        objectHashTextP :: Parser ObjectHash
        objectHashTextP = do
            bs <- P.take (constHashLength * 2)

            case maybeReadByteString bs of
                Nothing -> fail constErrorParseHashText
                Just h -> return h

        treeEntryP :: Parser TreeEntry
        treeEntryP = do
            Just mode <- readByteString <$> P.takeWhile isDigit <?> "file mode"
            space
            file <- P.takeWhile (/= 0) <?> "file name"
            word8 0

            hash <- objectHashP <?> "sub-object hash"

            return (TreeEntry mode file hash)

        treeObjectP :: Parser Object
        treeObjectP = fmap Tree (many treeEntryP <?> "tree entries")

        userStampP = do
            line <- manyTill anyWord8 newline <?> "user info"

            let separated = splitOn [toAscii ' '] line
            
            if length separated >= 2 then
                let info = intercalate [toAscii ' '] (List.take (length separated - 2) separated)
                    time = intercalate [toAscii ' '] (List.drop (length separated - 2) separated)

                in case parseOnly timestampP (SBS.pack time) of
                    Right time -> return (UserStamp (SBS.pack info) time)
                    Left err -> fail err
            else
                fail constErrorWrongTimestamp

        timestampP = do
            time_raw <- P.takeWhile isDigit; space
            sign <- satisfy (inClass "+-") <?> "time zone sign"
            zone_raw <- P.take 4 <?> "time zone delta"
            
            time <- parseTimeM True defaultTimeLocale "%s" (decodeUTF8 time_raw)
            zone <- parseTimeM True defaultTimeLocale "%z" (decodeUTF8 (SBS.cons sign zone_raw))

            return time {
                zonedTimeZone = zone
            }

        commitObjectP :: Parser Object
        commitObjectP = do
            tree <- treeP <?> "tree header"
            
            mparent <- fmap Just (parentP <?> "parent header") <|> return Nothing

            author <- authorP <?> "author header"
            committer <- committerP <?> "committer header"
            newline
            
            msg <- takeLazyByteString

            return (Commit tree mparent author committer msg)

            where
                treeP = do
                    string constCommitHeaderTree; space
                    tree <- objectHashTextP; newline
                    return tree

                parentP = do
                    string constCommitHeaderParent; space
                    hash <- objectHashTextP; newline
                    return hash

                authorP = do
                    string constCommitHeaderAuthor; space
                    stamp <- userStampP
                    return stamp

                committerP = do
                    string constCommitHeaderCommitter; space
                    stamp <- userStampP
                    return stamp

        tagObjectP :: Parser Object
        tagObjectP = do
            string constTagHeaderObject; space
            obj <- objectHashTextP; newline

            string constTagHeaderType; space
            header <- P.takeWhile (not . isNewline); newline

            ttype <-
                if header == constObjectBlobHeader then
                    return BlobType
                else if header == constObjectTreeHeader then
                    return TreeType
                else if header == constObjectCommitHeader then
                    return CommitType
                else if header == constObjectTagHeader then
                    return TagType
                else
                    fail (constErrorNoSuchObjectType (decodeUTF8 header))

            string constTagHeaderName; space
            name <- P.takeWhile (not . isNewline); newline

            string constCommitHeaderCommitter; space
            tagger <- userStampP

            newline

            msg <- takeLazyByteString

            return (Tag obj ttype name tagger msg)

        objectP :: Parser Object
        objectP = do
            header <- P.takeWhile (not . isSpace) <?> "object type"
            space
            length <- P.takeWhile isDigit <?> "object length"
            word8 0 <?> "header tag end"

            if header == constObjectBlobHeader then
                blobObjectP <?> "blob body"
            else if header == constObjectTreeHeader then
                treeObjectP <?> "tree body"
            else if header == constObjectCommitHeader then
                commitObjectP <?> "commit body"
            else if header == constObjectTagHeader then
                tagObjectP <?> "tag body"
            else
                fail (constErrorNoSuchObjectType (decodeUTF8 header))
