module Git.Object where

import Data.Time
import Data.List
import Data.Monoid
import qualified Data.ByteString as BS
import Data.Hashable
import Data.Char
import Data.List.Split

import Crypto.Hash.SHA1 as SHA1

import Text.Parsec
import Text.Parsec.ByteString

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
        if length input == constHashLength * 2 && all isHexDigit input then
            let raw =
                    BS.pack $ flip map [0,2..constHashLength - 1] $ \i ->
                        let [(val, _)] = readHex [input !! i, input !! (i + 1)]
                        in fromIntegral val
            in [(ObjectHash raw, "")]
        else []

instance Hashable ObjectHash where
    hashWithSalt salt (ObjectHash hash) = hashWithSalt salt hash

hashObject :: Object -> ObjectHash
hashObject = ObjectHash . SHA1.hash . encodeObject

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
            Just hash -> ["parent " ++ show hash]

        cont = fromString (headers ++ "\n\n" ++ msg ++ "\n")
        
        headers = intercalate "\n" $ [
                "tree " ++ show tree
            ] ++ parent ++ [
                "author " ++ encodeUserStamp author,
                "committer " ++ encodeUserStamp committer
            ]

-- parsers
parseBlobObject :: Parser Object
parseBlobObject = fmap (Blob . fromString) (many anyChar)

parseObjectHash :: Parser ObjectHash
parseObjectHash = fmap (ObjectHash . fromString) (count constHashLength anyChar)

parseTreeEntry :: Parser TreeEntry
parseTreeEntry = do
    mode <- many digit
    space
    file <- many (noneOf ['\0'])
    char '\0'

    hash <- parseObjectHash

    return (TreeEntry (read mode) file hash)

parseTreeObject :: Parser Object
parseTreeObject = fmap Tree (many parseTreeEntry)

parseCommitObject :: Parser Object
parseCommitObject = do
    tree <- treeP
    mparent <- optionMaybe parentP

    (author, committer) <- stampsP
    msg <- manyTill anyChar eof

    return (Commit tree mparent author committer msg)

    where
        treeP = do
            string "tree "
            tree <- hashTextP
            newline
            return tree

        parentP = do
            string "parent "
            hash <- hashTextP
            newline
            return hash

        authorP = do
            string "author "
            stamp <- userStampP
            return stamp

        committerP = do
            string "committer "
            stamp <- userStampP
            return stamp

        hashTextP = fmap read (count (constHashLength * 2) hexDigit)
        userStampP = do
            line <- manyTill anyChar newline

            let separated = splitOn " " line
            
            if length separated >= 2 then
                let info = take (length separated - 2) separated
                    time = drop (length separated - 2) separated
                in case parse timestampP "timestamp" (intercalate " " time) of
                    Right time -> return (UserStamp (intercalate " " info) time)
                    Left err -> fail (show err)
            else
                fail constErrorWrongTimestamp

        timestampP = do
            time_raw <- many digit
            space
            sign <- char '+' <|> char '-'
            zone_raw <- count 4 digit
            
            time <- parseTimeM True defaultTimeLocale "%s" time_raw
            zone <- parseTimeM True defaultTimeLocale "%z" (sign:zone_raw)

            return time {
                zonedTimeZone = zone
            }

        stampsP = (do
                committer <- committerP; author <- authorP;
                return (author, committer))
              <|> (do
                author <- authorP; committer <- committerP;
                return (author, committer))

parseObject :: Parser Object
parseObject = do
    header <- many letter
    space
    length <- many digit
    char '\0'

    if header == constObjectBlobHeader then
        parseBlobObject
    else if header == constObjectTreeHeader then
        parseTreeObject
    else if header == constObjectCommitHeader then
        parseCommitObject
    else
        fail (constErrorNoSuchObjectType header)
