module Scriptzzz.App.Controller.Handler
  ( HandleMessage
  , HandleScriptzzzMessage
  , MessageHandlingResult
  , failure
  , success
  ) where

import Scriptzzz.Prelude

import Scriptzzz.App.Command as Cmd
import Scriptzzz.App.Model (Model)

type HandleMessage model cmd payload =
  model → payload → MessageHandlingResult model cmd

type MessageHandlingResult model cmd = String \/ model /\ cmd

type HandleScriptzzzMessage ∷ ∀ k1 k2. k1 → k2 → Type → Type
type HandleScriptzzzMessage w h payload =
  HandleMessage
    (Model w h)
    (Cmd.Commands w h)
    payload

failure ∷ ∀ cmd model. String → MessageHandlingResult model cmd
failure = Left

success
  ∷ ∀ cmd model
  . { commands ∷ cmd, newModel ∷ model }
  → MessageHandlingResult model cmd
success { newModel, commands } = Right $ newModel /\ commands

