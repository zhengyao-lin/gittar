module Git.Object where

import Data.Time
import Data.List as List
import Data.Monoid
import qualified Data.ByteString.Lazy as BS
import Data.Hashable
import qualified Data.Char as Char
import Data.List.Split
import Data.Attoparsec.ByteString.Lazy as P

import Data.Ascii.Word8 (ascii, toChar)
import Data.Word8

import Debug.Trace

import Crypto.Hash.SHA1 as SHA1

import Control.Applicative

import Numeric

import Git.Utils
import Git.Consts

type TreeEntryMode = Int
data TreeEntry = TreeEntry TreeEntryMode String ObjectHash deriving (Show)

data UserStamp = UserStamp String ZonedTime deriving (Show)

data Object =
    Blob BS.ByteString |
    Tree [TreeEntry] |
    Commit {
        commitTree :: ObjectHash,
        commitParent :: Maybe ObjectHash,
        commitAuthor :: UserStamp,
        commitIssuer :: UserStamp,
        commitMsg :: String
    }
    deriving (Show)

newtype ObjectHash = ObjectHash BS.ByteString deriving (Eq, Ord)

instance Show ObjectHash where
    show (ObjectHash hash) = concat $ do
        c <- BS.unpack hash
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
                    BS.pack $ flip map [0,2..constHashLength * 2 - 1] $ \i ->
                        let [(val, _)] = readHex [input !! i, input !! (i + 1)]
                        in fromIntegral val
            in [(ObjectHash raw, "")]
        else []

instance Hashable ObjectHash where
    hashWithSalt salt (ObjectHash hash) = hashWithSalt salt hash

hashObject :: Object -> ObjectHash
hashObject = ObjectHash . toLazyByteString . SHA1.hashlazy . encodeObject

-- encoders
encodeObjectHash :: ObjectHash -> BS.ByteString
encodeObjectHash (ObjectHash hash) = hash

encodeTreeEntry :: TreeEntry -> BS.ByteString
encodeTreeEntry (TreeEntry mode name hash) =
    BS.append (fromString (show mode ++ " " ++ name ++ "\0")) (encodeObjectHash hash)

encodeUserStamp :: UserStamp -> String
encodeUserStamp (UserStamp info time) =
    intercalate " " [
        info,
        formatTime defaultTimeLocale "%s" time, -- unix timestamp
        timeZoneOffsetString (zonedTimeZone time)
    ]

-- general encoding for all object types
encodeObjectTemplate :: String -> BS.ByteString -> BS.ByteString
encodeObjectTemplate header cont =
    BS.append (fromString (header ++ " " ++ show (BS.length cont) ++ "\0")) cont

encodeObject :: Object -> BS.ByteString
encodeObject (Blob cont) = encodeObjectTemplate constObjectBlobHeader cont
encodeObject (Tree entries) = encodeObjectTemplate constObjectTreeHeader cont
    where cont = BS.concat (map encodeTreeEntry entries)

encodeObject (Commit {
    commitTree = tree,
    commitParent = mparent,
    commitAuthor = author,
    commitIssuer = committer,
    commitMsg = msg
}) =
    encodeObjectTemplate constObjectCommitHeader cont
    where
        parent = case mparent of
            Nothing -> []
            Just hash -> [constCommitHeaderParent ++ " " ++ show hash]

        cont = fromString (headers ++ "\n" ++ msg)
        
        headers = unlines $ [
                constCommitHeaderTree ++ " " ++ show tree
            ] ++ parent ++ [
                constCommitHeaderAuthor ++ " " ++ encodeUserStamp author,
                constCommitHeaderCommitter ++ " " ++ encodeUserStamp committer
            ]

parseObject :: BS.ByteString -> Either String Object
parseObject raw =
    case parseOnly objectP (toStrictByteString raw) of
        Right obj -> return obj
        Left err -> fail err

    where
        digit = satisfy isDigit
        hexDigit = satisfy isHexDigit
        space = satisfy isSpace
        letter = satisfy isAlpha
        newline = satisfy (== ascii '\n')
        stringRaw = string . toStrictByteString . fromString

        blobObjectP :: Parser Object
        blobObjectP = fmap Blob takeLazyByteString

        objectHashP :: Parser ObjectHash
        objectHashP = fmap (ObjectHash . toLazyByteString) (P.take constHashLength)

        treeEntryP :: Parser TreeEntry
        treeEntryP = do
            mode <- map toChar <$> many digit
            space
            file <- map toChar <$> many (notWord8 0)
            word8 0

            hash <- objectHashP

            return (TreeEntry (read mode) file hash)

        treeObjectP :: Parser Object
        treeObjectP = fmap Tree (many treeEntryP)

        commitObjectP :: Parser Object
        commitObjectP = do
            tree <- treeP
            mparent <- fmap Just parentP <|> return Nothing

            (author, committer) <- stampsP; newline
            
            msg <- takeLazyByteString

            return (Commit tree mparent author committer (toString msg))

            where
                treeP = do
                    stringRaw constCommitHeaderTree; space
                    tree <- hashTextP; newline
                    return tree

                parentP = do
                    stringRaw constCommitHeaderParent; space
                    hash <- hashTextP; newline
                    return hash

                authorP = do
                    stringRaw constCommitHeaderAuthor; space
                    stamp <- userStampP
                    return stamp

                committerP = do
                    stringRaw constCommitHeaderCommitter; space
                    stamp <- userStampP
                    return stamp

                hashTextP = fmap (read . map toChar) (count (constHashLength * 2) hexDigit)
                userStampP = do
                    line <- manyTill anyWord8 newline

                    let separated = splitOn [ascii ' '] line
                    
                    if length separated >= 2 then
                        let info = intercalate [ascii ' '] (List.take (length separated - 2) separated)
                            time = intercalate [ascii ' '] (List.drop (length separated - 2) separated)

                        in case parseOnly timestampP (toStrictByteString (BS.pack time)) of
                            Right time -> return (UserStamp (map toChar info) time)
                            Left err -> fail err
                    else
                        fail constErrorWrongTimestamp

                timestampP = do
                    time_raw <- many digit; space
                    sign <- satisfy (inClass "+-")
                    zone_raw <- count 4 digit
                    
                    time <- parseTimeM True defaultTimeLocale "%s" (map toChar time_raw)
                    zone <- parseTimeM True defaultTimeLocale "%z" (map toChar (sign:zone_raw))

                    return time {
                        zonedTimeZone = zone
                    }

                stampsP = (do
                        author <- authorP; committer <- committerP;
                        return (author, committer))
                    <|> (do
                        committer <- committerP; author <- authorP;
                        return (author, committer))

        objectP :: Parser Object
        objectP = do
            header <- map toChar  <$> many letter
            space
            length <- many digit
            word8 0

            if header == constObjectBlobHeader then
                blobObjectP
            else if header == constObjectTreeHeader then
                treeObjectP
            else if header == constObjectCommitHeader then
                commitObjectP
            else
                fail (constErrorNoSuchObjectType header)
