import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMainLog)
import Core.Settings        (parseExtra)
import Core.Application     (makeApplication)

main :: IO ()
main = defaultMainLog (fromArgs parseExtra) makeApplication
