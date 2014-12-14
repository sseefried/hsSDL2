module Paths_SDL2 (
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
version = Version {versionBranch = [0,1,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/sseefried/local/ghc-ios-i386/bin"
libdir     = "/Users/sseefried/local/ghc-ios-i386/lib/i386-ios-ghc-7.8.3/SDL2-0.1.0"
datadir    = "/Users/sseefried/local/ghc-ios-i386/share/i386-ios-ghc-7.8.3/SDL2-0.1.0"
libexecdir = "/Users/sseefried/local/ghc-ios-i386/libexec"
sysconfdir = "/Users/sseefried/local/ghc-ios-i386/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SDL2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SDL2_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "SDL2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SDL2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SDL2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
