module TDLib.Types.Common where

import Control.Applicative
import Data.Aeson

data a ∪ b
  = Inl a
  | Inr b
  deriving (Show, Eq)

instance (FromJSON a, FromJSON b) => FromJSON (a ∪ b) where
  parseJSON a =
    Inl <$> parseJSON a <|> Inr <$> parseJSON a

-- | Logging verbosity
data Verbosity
  = Fatal
  | Error
  | Warning
  | Info
  | Debug
  | Verbose
  deriving (Show, Eq, Enum)
