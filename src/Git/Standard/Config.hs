module Git.Standard.Config where

import Data.Ini
import qualified Data.Text as T

import System.IO
import System.FilePath

import Git.Standard.Consts

data StandardConfig = StandardConfig {
    repoFormatVersion :: String
}

readStandardConfig :: FilePath -> IO (Either String StandardConfig)
readStandardConfig path = do
    res <- readIniFile path

    return $ res >>= \ini -> do
        version <- lookupValue
            (T.pack constConfigCoreSection)
            (T.pack constConfigCoreSectionVersion) ini
        
        return StandardConfig {
            repoFormatVersion = T.unpack version
        }
