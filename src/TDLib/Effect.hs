{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module TDLib.Effect where

import Data.Aeson
import Data.ByteString
import Data.Int
import Data.Kind
import Polysemy
import TDLib.Types.Common

data TDLib (m :: Type -> Type) a where
  RunCmd :: (ToJSON cmd, FromJSON res) => cmd -> TDLib m res
  SetVerbosity :: Verbosity -> TDLib m ()
  SetFatalErrorCallback :: (ByteString -> IO ()) -> TDLib m ()
  SetLogPath :: ByteString -> TDLib m Bool
  SetLogMaxSize :: Int64 -> TDLib m ()

makeSem_ ''TDLib

-- | runs a command and waits fot its result
runCmd ::
  forall cmd res r.
  (ToJSON cmd, FromJSON res, Member TDLib r) =>
  -- | Command
  cmd ->
  Sem r res

setVerbosity :: forall r. Member TDLib r => Verbosity -> Sem r ()

setFatalErrorCallback :: forall r. Member TDLib r => (ByteString -> IO ()) -> Sem r ()

setLogPath :: forall r. Member TDLib r => ByteString -> Sem r Bool

setLogMaxSize :: forall r. Member TDLib r => Int64 -> Sem r ()
