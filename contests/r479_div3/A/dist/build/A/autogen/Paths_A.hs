{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_A (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/pohvalister/.cabal/bin"
libdir     = "/home/pohvalister/.cabal/lib/x86_64-linux-ghc-8.2.2/A-0.1.0.0-9YUYVCTjMmY38TNJlbTpTc-A"
dynlibdir  = "/home/pohvalister/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/pohvalister/.cabal/share/x86_64-linux-ghc-8.2.2/A-0.1.0.0"
libexecdir = "/home/pohvalister/.cabal/libexec/x86_64-linux-ghc-8.2.2/A-0.1.0.0"
sysconfdir = "/home/pohvalister/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "A_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "A_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "A_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "A_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "A_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "A_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
