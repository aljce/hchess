module Paths_hchess (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kyle/Programming/haskell/hchess/.stack-work/install/x86_64-linux/nightly-2015-11-14/7.10.2/bin"
libdir     = "/home/kyle/Programming/haskell/hchess/.stack-work/install/x86_64-linux/nightly-2015-11-14/7.10.2/lib/x86_64-linux-ghc-7.10.2/hchess-0.1.0.0-DCOOKx5aUDOAR8ciXX95kw"
datadir    = "/home/kyle/Programming/haskell/hchess/.stack-work/install/x86_64-linux/nightly-2015-11-14/7.10.2/share/x86_64-linux-ghc-7.10.2/hchess-0.1.0.0"
libexecdir = "/home/kyle/Programming/haskell/hchess/.stack-work/install/x86_64-linux/nightly-2015-11-14/7.10.2/libexec"
sysconfdir = "/home/kyle/Programming/haskell/hchess/.stack-work/install/x86_64-linux/nightly-2015-11-14/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hchess_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hchess_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hchess_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hchess_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hchess_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
