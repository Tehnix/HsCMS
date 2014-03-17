module Core.Model where

import Prelude
import Yesod hiding (Content)
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)

-- Custom imports
import Data.Time (UTCTime)
import Core.Types


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
