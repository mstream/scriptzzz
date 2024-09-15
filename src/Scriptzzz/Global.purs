module Scriptzzz.Global (Scriptzzz, appId, sendMessage) where

import Prelude

import Effect (Effect)
import Flame (AppId(..))
import Flame.Subscription (send)
import Scriptzzz.App.Message (Message)

data Scriptzzz = Scriptzzz

instance Show Scriptzzz where
  show Scriptzzz = "scriptzzz"

appId :: AppId Scriptzzz Message 
appId = AppId Scriptzzz

sendMessage :: Message -> Effect Unit
sendMessage = send appId 


