module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Facebook.Sdk (FbConfig, defaultFacebookConfig, facebookInit, facebookLoginStatus)
import Prelude (Unit, bind, discard, ($))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Karma (runKarma)

config :: FbConfig
config = defaultFacebookConfig "320125848412942"

foreign import sleep :: ∀ e. Int -> Eff e Unit

main :: ∀ e. Eff ( console :: CONSOLE , testOutput :: TESTOUTPUT , avar :: AVAR , timer :: TIMER | e ) Unit
main = runKarma do
  suite "facebook sdk" $ do
    test "facebook init" $ do
      log "before init"
      fb <- facebookInit config
      info <- facebookLoginStatus fb
      liftEff $ sleep 15000
      Assert.assert "should reach here" true
