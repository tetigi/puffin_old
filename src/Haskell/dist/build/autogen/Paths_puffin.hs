module Paths_puffin (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/tetigi/.cabal/bin"
libdir     = "/home/tetigi/.cabal/lib/puffin-0.1/ghc-7.4.1"
datadir    = "/home/tetigi/.cabal/share/puffin-0.1"
libexecdir = "/home/tetigi/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "puffin_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "puffin_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "puffin_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "puffin_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
