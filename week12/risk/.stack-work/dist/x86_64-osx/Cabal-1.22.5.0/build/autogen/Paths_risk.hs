module Paths_risk (
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

bindir     = "/Users/ismailmustafa/Desktop/HaskellCourse/CIS-194/week12/risk/.stack-work/install/x86_64-osx/lts-3.19/7.10.3/bin"
libdir     = "/Users/ismailmustafa/Desktop/HaskellCourse/CIS-194/week12/risk/.stack-work/install/x86_64-osx/lts-3.19/7.10.3/lib/x86_64-osx-ghc-7.10.3/risk-0.1.0.0-8R0h6jPH9CL3nM9BWfn0oC"
datadir    = "/Users/ismailmustafa/Desktop/HaskellCourse/CIS-194/week12/risk/.stack-work/install/x86_64-osx/lts-3.19/7.10.3/share/x86_64-osx-ghc-7.10.3/risk-0.1.0.0"
libexecdir = "/Users/ismailmustafa/Desktop/HaskellCourse/CIS-194/week12/risk/.stack-work/install/x86_64-osx/lts-3.19/7.10.3/libexec"
sysconfdir = "/Users/ismailmustafa/Desktop/HaskellCourse/CIS-194/week12/risk/.stack-work/install/x86_64-osx/lts-3.19/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "risk_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "risk_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "risk_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "risk_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "risk_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
