import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Core.Settings             (parseExtra)
import Core.Application          (makeApplication)

main :: IO ()
main = defaultMain (fromArgs parseExtra) makeApplication
