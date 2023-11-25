{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_CardGame (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/liyanran/.cabal/bin"
libdir     = "/Users/liyanran/.cabal/lib/aarch64-osx-ghc-9.4.7/CardGame-0.1.0.0-inplace-CardGame"
dynlibdir  = "/Users/liyanran/.cabal/lib/aarch64-osx-ghc-9.4.7"
datadir    = "/Users/liyanran/.cabal/share/aarch64-osx-ghc-9.4.7/CardGame-0.1.0.0"
libexecdir = "/Users/liyanran/.cabal/libexec/aarch64-osx-ghc-9.4.7/CardGame-0.1.0.0"
sysconfdir = "/Users/liyanran/.cabal/etc"

getBinDir     = catchIO (getEnv "CardGame_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "CardGame_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "CardGame_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "CardGame_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CardGame_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CardGame_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
