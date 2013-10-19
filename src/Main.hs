
-- HsApp:  A Generic console application, for testing miscelaneous facilities 
--         @2013 Angel Alvarez,
-- 

module Main where

import Data.Maybe ( fromMaybe )
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Trans.Either
import System.Environment ( getArgs )
import System.FilePath
import System.IO
import System.Cmd ( system )
import System.Console.GetOpt

-- Cabal imports
import Data.Version (showVersion)
import Distribution.Version
import Paths_HsApp as HsApp

import Options
import OptsCheck

progName = "Universal Parser, Version:" ++ currVersion ++ " "
    where
        currVersion :: String
        currVersion = showVersion HsApp.version

progAuthors = "@2013 Angel Alvarez"

-- default options
defaultOptions    = Options
 { optDump        = False
 , optModules     = [("print",printFiles)]
 , optMode        = Nothing
 , optVerbose     = False
 , optShowVersion = False
 , optOutput      = Nothing
 , optDataDir     = Nothing
 , optInput       = []
 }

-- currently supported options
acceptedOptions :: [OptsPolicy]
acceptedOptions =
 [ 
   Option ['h','?'] ["help"]    (NoArg  ( check_help           ))                "Show this help message."
 , Option ['V']     ["Version"] (NoArg  ( check_version        ))                "Show version number"
 , Option ['D']     ["datadir"] (ReqArg ( check_data_dir       ) "Dir")          "Directory where files are located"
 , Option ['m']     ["mode"]    (ReqArg ( check_operation_mode ) "Mode")         "Mode of Operation"
 , Option []        ["dump"]    (NoArg  ( check_dump_options   ))                "Force args cmdline dump"
--  , Option ['e']     ["error"]   (NoArg (\ _opts -> return $ Left "forced error on args detected!"))  "Force args checking error"
--  , Option ['v']     ["verbose"] (NoArg (\ opts -> check_verbosity opts))                         "Verbose run on stderr"
--  , Option ['i']     ["input"]   (OptArg (\f opts -> check_input_file f opts) "FILE")             "Input file"
 ]


main :: IO ()
main = do
    args   <- getArgs
    cores  <- getNumCapabilities
    progHeader cores
    result <- runEitherT $ progOpts args defaultOptions acceptedOptions  -- thread Options over monadic checkers using EitherT over IO
    either somethingIsWrong doSomeStuff result


somethingIsWrong :: String -> IO ()    
somethingIsWrong msg = do
             putStrLn $ "\nError: " ++ msg ++ "\n"
             putStrLn $ usageInfo header acceptedOptions

doSomeStuff :: Options -> IO ()
doSomeStuff optsR@Options { optMode = mode } = do
    case mode of
         Nothing -> doNothing optsR
         Just fun -> fun optsR
         

printFiles :: Options -> IO ()
printFiles opts@Options { optInput = files, optDataDir = datadir } = do
    mapM_ printargs filepaths 
    where
            dir = fromMaybe "./" datadir
            filepaths = zipWith (combine) (cycle [dir]) files
            printargs :: String -> IO ()
            printargs path = putStrLn $ "Processing path: " ++ path ++ "..."

doNothing :: Options -> IO ()
doNothing _ = somethingIsWrong "There is not payload module to execute!\n"             

   -- Keep calm and curry on, we are the good guys....
progHeader :: Int -> IO ()
progHeader c = 
    putStrLn $ progName ++" " ++ progAuthors ++ "\n" ++ show(c) ++ " processor " ++ (core2string c) ++ " detected."
    where
        core2string :: Int -> String
        core2string c = case c > 1 of
                             True -> "cores"
                             False -> "core"

header :: String
header = "Usage: Options [OPTION...] files..."

