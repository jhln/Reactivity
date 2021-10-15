{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Reactivity (
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
libdir     = "/home/johannes/.cabal/lib/x86_64-linux-ghc-8.8.4/Reactivity-0.1.0.0-inplace-Reactivity"
dynlibdir  = "/home/johannes/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/johannes/.cabal/share/x86_64-linux-ghc-8.8.4/Reactivity-0.1.0.0"
libexecdir = "/home/johannes/.cabal/libexec/x86_64-linux-ghc-8.8.4/Reactivity-0.1.0.0"
sysconfdir = "/home/johannes/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Reactivity_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Reactivity_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Reactivity_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Reactivity_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Reactivity_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Reactivity_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
