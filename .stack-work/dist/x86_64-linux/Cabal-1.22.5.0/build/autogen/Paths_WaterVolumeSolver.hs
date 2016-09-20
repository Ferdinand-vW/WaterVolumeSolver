module Paths_WaterVolumeSolver (
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

bindir     = "/home/ferdinand/Projects/WaterVolumeSolver/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/bin"
libdir     = "/home/ferdinand/Projects/WaterVolumeSolver/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/lib/x86_64-linux-ghc-7.10.3/WaterVolumeSolver-0.1.0.0-95FBMSur5uJLWmCvPCHuCl"
datadir    = "/home/ferdinand/Projects/WaterVolumeSolver/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/share/x86_64-linux-ghc-7.10.3/WaterVolumeSolver-0.1.0.0"
libexecdir = "/home/ferdinand/Projects/WaterVolumeSolver/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/libexec"
sysconfdir = "/home/ferdinand/Projects/WaterVolumeSolver/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "WaterVolumeSolver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "WaterVolumeSolver_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "WaterVolumeSolver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "WaterVolumeSolver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "WaterVolumeSolver_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
