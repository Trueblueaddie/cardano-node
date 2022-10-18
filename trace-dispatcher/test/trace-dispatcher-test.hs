{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Control.Monad (when)
import           Data.Functor.Identity
import           System.Directory (removeDirectoryRecursive)
import           System.PosixCompat.Files (fileExist)
import           System.Process (cleanupProcess, spawnProcess)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Data.IORef (IORef, newIORef)

import           Cardano.Api (SocketPath (..))
import           Cardano.Logging
import           Cardano.Logging.ForwardingStressTest.Script
import           Cardano.Logging.Test.Oracles
import           Cardano.Logging.Test.Script
import           Ouroboros.Network.NodeToClient (withIOManager)


main :: IO ()
main = do
    ts <- getTestSetup
    let logPath = "/tmp/cardano-forwarder-test-logs"
    fe <- fileExist logPath
    when fe
        (removeDirectoryRecursive logPath)
    cardanoTracerHdl <- spawnProcess "cardano-tracer"
        [ "-c"
        , "test/cardano-tracer-config.yaml"]
    threadDelay 1000000 --wait 1 seconds
    accumulationCounter <- newIORef 0
    fwdTracer <- do
        -- TODO: check if this is the correct way to use withIOManager
        (forwardSink, _dpStore) <- withIOManager $ \iomgr -> do
            -- For simplicity, we are always 'Initiator',
            -- so 'cardano-tracer' is always a 'Responder'.
            let tracerSocketMode = Just (runIdentity (tsSocketPath ts), Initiator)
            initForwarding iomgr simpleTestConfig (runIdentity (tsNetworkMagic ts)) Nothing tracerSocketMode
        pure (forwardTracer forwardSink)
    stdoutTracer' <- standardTracer

    defaultMain (allTests ts fwdTracer stdoutTracer' accumulationCounter)
        `catch` (\ (e :: SomeException) -> do
            cleanupProcess (Nothing, Nothing, Nothing, cardanoTracerHdl)
            throwIO e)


allTests ::
     TestSetup Identity
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> IORef Int
  -> TestTree
allTests ts fwdTracer stdoutTracer' accumulationCounter =
    testGroup "Tests"
        [ localTests
        , forwarderTests ts fwdTracer stdoutTracer' accumulationCounter
        ]

forwarderTests ::
     TestSetup Identity
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> IORef Int
  -> TestTree
forwarderTests ts fwdTracer stdoutTracer' accumulationCounter =
    localOption (QuickCheckTests 10) $ testGroup "trace-forwarder"
    [ testProperty "multi-threaded forwarder stress test" $
        runScriptForwarding ts fwdTracer stdoutTracer' accumulationCounter
    ]

localTests :: TestTree
localTests = localOption (QuickCheckTests 10) $ testGroup "trace-dispatcher"
    [ testProperty "single-threaded send tests" $
        runScriptSimple 1.0 oracleMessages
    , testProperty "multi-threaded send tests" $
        runScriptMultithreaded 1.0 oracleMessages
    -- , testProperty "multi-threaded send tests with reconfiguration" $
    --     runScriptMultithreadedWithReconfig 1.0 oracleMessages
    , testProperty "reconfiguration stress test" $
        runScriptMultithreadedWithConstantReconfig 1.0 (\ _ _ -> property True)
    ]