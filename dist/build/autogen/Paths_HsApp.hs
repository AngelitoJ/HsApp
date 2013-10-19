module Paths_HsApp (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/sinosuke/.cabal/bin"
libdir     = "/home/sinosuke/.cabal/lib/HsApp-0.1.0.0/ghc-7.4.2"
datadir    = "/home/sinosuke/.cabal/share/HsApp-0.1.0.0"
libexecdir = "/home/sinosuke/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "HsApp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HsApp_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HsApp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HsApp_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
