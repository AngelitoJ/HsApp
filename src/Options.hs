
-- HsApp:  A Generic console application, for testing miscelaneous facilities 
--         @2013 Angel Alvarez,
-- 

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



-- An EitherT container to store parsed opts from commandline and posible error messages
type OptsResult = EitherT String IO Options

-- An Opts filter is an Options processing function running inside the EitherT IO Stack
type OptsFilter = ( Options -> OptsResult )

-- A Policy describes a getopt command line descriptor along checking filter
type OptsPolicy = OptDescr OptsFilter


-- Record for storing cmdline options
data Options = Options
 { 
 	  optDump        :: Bool                         -- flag to force a options record printout
 	, optModules     :: [(String,(Options-> IO()))]  -- posible payloads that this program carry
 	, optMode        :: Maybe (Options->IO())        -- the selected payload module among all in optModules
 	, optVerbose     :: Bool                         -- simple verbose flag
 	, optShowVersion :: Bool                         -- flag to indicate a version printout
 	, optOutput      :: Maybe FilePath               -- Optional path to output results
 	, optDataDir     :: Maybe FilePath               -- Optional dir to base all file lookups
 	, optInput       :: [FilePath]                   -- List of arguments (TODO: maybe file or other types, right now they are treated like files)
 } deriving (Show)
