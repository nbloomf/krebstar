module Kreb.Editor.Signal (
    AppSignal(..)
) where

import Kreb.Lang

data AppSignal
  = ExitNormally
  | StdLibReadError IOError
  | StdLibParseError Error
  | StdLibInterpretError ReplError
  deriving (Eq, Show)
