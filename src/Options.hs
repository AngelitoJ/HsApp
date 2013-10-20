module Options 
	(
		  OptsResult(..)
		, OptsFilter(..)
		, OptsPolicy(..)
		, Options(..)
	) where

import Control.Monad.Trans.Either
import System.Console.GetOpt
import Text.Show.Functions



-- An EitherT container to store parsed opts from commandline or error messages
type OptsResult = EitherT String IO Options

-- An Opts filter runnng in the EitherT IO Stack
type OptsFilter = ( Options -> OptsResult )

-- A Policy describing a command line options with a checking filter
type OptsPolicy = OptDescr OptsFilter



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
