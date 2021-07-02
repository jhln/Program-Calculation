{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ProgramCalculation (
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

bindir     = "/home/johannes/.cabal/bin"
libdir     = "/home/johannes/.cabal/lib/x86_64-linux-ghc-8.8.4/ProgramCalculation-0.1.0.0-inplace-ProgramCalculation"
dynlibdir  = "/home/johannes/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/johannes/.cabal/share/x86_64-linux-ghc-8.8.4/ProgramCalculation-0.1.0.0"
libexecdir = "/home/johannes/.cabal/libexec/x86_64-linux-ghc-8.8.4/ProgramCalculation-0.1.0.0"
sysconfdir = "/home/johannes/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ProgramCalculation_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ProgramCalculation_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ProgramCalculation_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ProgramCalculation_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ProgramCalculation_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ProgramCalculation_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
