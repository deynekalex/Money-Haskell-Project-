module Paths_Money (
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

bindir     = "/home/deynekalex/Desktop/Haskell/Money/.stack-work/install/x86_64-linux/lts-5.13/7.10.3/bin"
libdir     = "/home/deynekalex/Desktop/Haskell/Money/.stack-work/install/x86_64-linux/lts-5.13/7.10.3/lib/x86_64-linux-ghc-7.10.3/Money-0.1.0.0-GShlO9iRvaL96DfHp3PQ7V"
datadir    = "/home/deynekalex/Desktop/Haskell/Money/.stack-work/install/x86_64-linux/lts-5.13/7.10.3/share/x86_64-linux-ghc-7.10.3/Money-0.1.0.0"
libexecdir = "/home/deynekalex/Desktop/Haskell/Money/.stack-work/install/x86_64-linux/lts-5.13/7.10.3/libexec"
sysconfdir = "/home/deynekalex/Desktop/Haskell/Money/.stack-work/install/x86_64-linux/lts-5.13/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Money_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Money_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Money_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Money_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Money_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
