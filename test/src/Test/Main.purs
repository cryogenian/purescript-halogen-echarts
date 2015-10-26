module Test.Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff.Exception (throwException)
import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Indexed as H


main :: forall e. Eff (console :: CONSOLE|e) Unit
main = do
  log "FOO"
