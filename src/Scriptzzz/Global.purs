module Scriptzzz.Global (Scriptzzz, appId, sendMessage) where

import Prelude

import Effect (Effect)
import Flame (AppId(..))
import Flame.Subscription (send)
import Scriptzzz.App.Message as Msg

data Scriptzzz = Scriptzzz

instance Show Scriptzzz where
  show Scriptzzz = "scriptzzz"

appId :: AppId Scriptzzz Msg.Message 
appId = AppId Scriptzzz

sendMessage :: Msg.Message -> Effect Unit
sendMessage = send appId


