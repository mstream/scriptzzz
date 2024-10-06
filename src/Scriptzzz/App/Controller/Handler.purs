module Scriptzzz.App.Controller.Handler
  ( HandleMessage
  , MessageHandlingResult
  , failure
  , success
  ) where

import Scriptzzz.Prelude

type HandleMessage model cmd payload =
  model → payload → MessageHandlingResult model cmd

type MessageHandlingResult model cmd = String \/ model /\ cmd

failure ∷ ∀ cmd model. String → MessageHandlingResult model cmd
failure = Left

success
  ∷ ∀ cmd model
  . { commands ∷ cmd, newModel ∷ model }
  → MessageHandlingResult model cmd
success { newModel, commands } = Right $ newModel /\ commands

