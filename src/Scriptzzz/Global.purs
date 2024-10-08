module Scriptzzz.Global (Scriptzzz, appId, sendMessage) where

import Prelude

import Data.Typelevel.Num (class Pos)
import Effect (Effect)
import Flame (AppId(..))
import Flame.Subscription (send)
import Scriptzzz.App.Message as Msg

data Scriptzzz = Scriptzzz

instance Show Scriptzzz where
  show Scriptzzz = "scriptzzz"

appId :: forall h w. Pos h => Pos w => AppId Scriptzzz (Msg.Message w h) 
appId = AppId Scriptzzz

sendMessage :: forall h w. Pos h => Pos w => Msg.Message w h -> Effect Unit
sendMessage = send appId


