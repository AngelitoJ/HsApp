
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


import Control.Monad(foldM,liftM,ap)
import Control.Monad.IO.Class
import Data.List (find)
import Data.Maybe ( fromMaybe )
import Data.Either

import System.Environment
import System.Console.GetOpt
import System.Directory ( doesDirectoryExist, doesFileExist )
import System.FilePath



-- Cabal imports
import Data.Version (showVersion)
import Distribution.Version
import Paths_HsApp as HsApp

import Options


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

-- =============================================== Options checking ==============================================
-- progOpts: getOpt args processor that also checks semantically getopt results or bang in front of the user
-- Upon checking with getopt this function gets a list of lambdas representing semantical checks
-- on every cmdline switch, as an example; we check for input file presence and the check that either data
-- director is a valid one and input file exists and is readable. Those checks are performed by
-- filename_check, check_datadir and check_input_file respectively. These funs are stored
-- in the Options structure that getopt uses.

--  We use a EitherT transformer to combine Either chaining with arbitrary IO actions needed during checking
-- ===============================================================================================================

progOpts :: [String] -> Options -> [OptsPolicy] -> OptsResult
progOpts args defaultOptions acceptedOptions =
   case getOpt RequireOrder acceptedOptions args of
      (funs,[],[]) -> do
          left "input file(s) missing"
      (funs,filenames,[]) -> do
          resultOfFuns <- foldl (>>=) (return defaultOptions) funs               -- Perform monadic checkings upon getOpt supplied functions
          foldM check_input_file resultOfFuns filenames                          -- Now check if all the input files exist and are accesible
      (_,_,errs) -> do
          left ( concat errs )


-- =============================================== Monadic Options checkers =======================================
-- getOpt will partially apply against the supplied argument do we can just come over the options record
          
          
-- Who we are?, sort of alien outbreak?
check_version :: Options -> OptsResult
check_version optsR = return $ optsR { optShowVersion = True }
          
-- help message, dont panic, we are just not going anywhere after showing the help
check_help :: Options -> OptsResult
check_help _ = left "Command line help requested"
    
--check supplied input files exist or bang if any is not.
check_input_file :: Options -> String -> OptsResult
check_input_file optsR@Options { optInput = files , optDataDir = dataDir } filename = do
    test <- liftIO $ filename_check dataDir filename
    case test of
         True -> return $ (optsR { optInput = filename : files })
         False -> left $ "input file "++ filename ++ " not readable"
    where
        filename_check :: Maybe FilePath -> FilePath -> IO Bool --check file with or without data directory
        filename_check (Just datadir) filename = doesFileExist $ combine datadir filename
        filename_check Nothing filename        = doesFileExist filename

-- User passed some directory, we make sure this dir exits so file will matched against it
check_data_dir :: String -> Options -> OptsResult
check_data_dir dir optsR = do
    test <- liftIO $ doesDirectoryExist dir
    case test of
         True -> return $ optsR { optDataDir =  Just dir } 
         False -> left ( "Data directory " ++ dir ++ " does not exist" )

-- check user wants verbosity level increased
check_verbosity :: Options -> OptsResult
check_verbosity optsR = return $ optsR { optVerbose = True }

-- check mode of operation. A list of modules is provided in the options record
check_operation_mode :: String -> Options -> OptsResult 
check_operation_mode mode optsR@Options { optModules = modules } = return $ optsR { optMode = selectedModule }
    where
        selectedModule = case (findmodule mode modules) of
                              Just (_,fun) -> Just fun
                              Nothing      -> Nothing
        findmodule :: String -> [(String, (Options-> IO()))] -> Maybe (String,(Options -> IO ()))
--         findmodule mode modules = find (\(x,_) -> x == mode ) modules 
        findmodule mode = find ((== mode).fst)  


-- dump either options or errors as we get passthrought
check_dump_options :: Options -> OptsResult
check_dump_options optsR = do
    liftIO $ putStrLn $ "\n\nOptions record dump selected: \n\t" ++ show optsR ++ "\n"
    return $ optsR { optDump =True } 



