module Options where

import Text.Show.Functions



-- Record for storing cmdline options
data Options = Options
 { optDump        :: Bool
 , optModules     :: [(String,(Options-> IO()))]
 , optMode        :: Maybe (Options->IO())
 , optVerbose     :: Bool
 , optShowVersion :: Bool
 , optOutput      :: Maybe FilePath
 , optDataDir     :: Maybe FilePath
 , optInput       :: [FilePath]
 } deriving (Show)
