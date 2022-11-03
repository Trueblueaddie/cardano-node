{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.ShutdownOnSlotSynced
  ( hprop_shutdownOnSlotSynced
  ) where

import           Prelude
import           Control.Monad
import           Data.Either (isRight)
import           Data.List (find, isInfixOf)
import           Data.Maybe
import           GHC.IO.Exception (ExitCode (ExitSuccess))
import           GHC.Stack (callStack)
import           System.FilePath ((</>))
import qualified System.Directory as IO
import           Text.Read (readMaybe) 

import           Hedgehog (Property, assert, (===))
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H

import           Testnet (TestnetOptions( CardanoOnlyTestnetOptions),  testnet)
import           Testnet.Cardano (TestnetNodeOptions (TestnetNodeOptions),
                   CardanoTestnetOptions (..), TestnetRuntime (..), defaultTestnetNodeOptions,
                   defaultTestnetOptions)
import qualified Testnet.Cardano as TC
import qualified Testnet.Conf as H
import qualified Util.Base as H
import           Util.Runtime (NodeRuntime (..))

hprop_shutdownOnSlotSynced :: Property
hprop_shutdownOnSlotSynced = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  -- Start a local test net
  base <- H.note =<< H.noteIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf <- H.noteShowM $
    H.mkConf (H.ProjectBase base) (H.YamlFilePath configurationTemplate) tempAbsBasePath' Nothing
  let maxSlot = 1500
      slotLen = 0.01
  let fastTestnetOptions = CardanoOnlyTestnetOptions $ defaultTestnetOptions
        { epochLength = 300
        , slotLength = slotLen
        , bftNodeOptions =
          [ TestnetNodeOptions
              { TC.extraNodeCliArgs = ["--shutdown-on-slot-synced", show maxSlot]
              }
          , defaultTestnetNodeOptions
          , defaultTestnetNodeOptions
          ]
        }
  TC.TestnetRuntime { bftNodes = node:_ } <- testnet fastTestnetOptions conf

  -- Wait for the node to exit
  let timeout :: Int
      timeout = round (40 + (fromIntegral maxSlot * slotLen))
  mExitCodeRunning <- H.waitSecondsForProcess timeout (nodeProcessHandle node)

  -- Check results
  when (isRight mExitCodeRunning) $ do
    H.cat (nodeStdout node)
    H.cat (nodeStderr node)
  mExitCodeRunning === Right ExitSuccess
  logs <- H.readFile (nodeStdout node)
  slotTip <- case find (isInfixOf "Closed db with immutable tip") (reverse (lines logs)) of
    Nothing -> H.failMessage callStack "Could not find current tip in node's log."
    Just line -> case listToMaybe (reverse (words line)) of
      Nothing -> H.failMessage callStack "Impossible"
      Just lastWord -> case readMaybe @Integer lastWord of
        Nothing -> H.failMessage callStack ("Expected a node tip as the last word of the log line, but got: " ++ line)
        Just slotTip -> H.noteShow slotTip

  let epsilon = 50
  assert (maxSlot <= slotTip && slotTip <= maxSlot + epsilon)
  return ()
