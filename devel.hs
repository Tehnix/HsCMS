{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
import "HsCMS" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

#ifndef mingw32_HOST_OS
import System.Posix.Signals (installHandler, sigINT, Handler(Catch))
#endif

main :: IO ()
main = do
#ifndef mingw32_HOST_OS
    _ <- installHandler sigINT (Catch $ return ()) Nothing
#endif

    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    forkIO $ runSettings (setPort port defaultSettings) app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "yesod-devel/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
