{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Logging.ForwardingStressTest.Script
  ( TestSetup(..)
  , simpleTestConfig
  , getTestSetup
  , runScriptForwarding
  ) where

import           Control.Concurrent (ThreadId, forkFinally, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception.Base (SomeException, throw)
import           Control.Monad (join, when)
import           Data.Functor ((<&>))
import           Data.Functor.Identity
import           Data.IORef
import           Data.List (sort)
import           Data.Map (fromList)
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics (Generic)
import           Generic.Data (gmappend)
import           Options.Applicative
import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Test.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Config ()
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Types


import           Debug.Trace


data TestSetup a
  = TestSetup
  { tsTime         :: !(a Double)
  , tsThreads      :: !(a Int)
  , tsMessages     :: !(a (Maybe Int))
  , tsSocketPath   :: !(a FilePath)
  , tsNetworkMagic :: !(a NetworkMagic)
  } deriving (Generic)
instance Semigroup (TestSetup Last) where
  (<>) = gmappend

deriving instance Show (TestSetup Identity)

defaultTestSetup :: TestSetup Last
defaultTestSetup =
  TestSetup
  { tsTime         = Last $ Just 10.0
  , tsThreads      = Last $ Just 5
  , tsMessages     = Last   Nothing
  , tsSocketPath   = Last $ Just "/tmp/tracer.sock"
  , tsNetworkMagic = Last $ Just testnetMagic
  }
 where testnetMagic = NetworkMagic 764824073

parseTestSetup :: Parser (TestSetup Last)
parseTestSetup =
  TestSetup
  <$> (Last <$> optional (option auto (long "time"          <> metavar "SEC")))
  <*> (Last <$> optional (option auto (long "threads"       <> metavar "THRDS")))
  <*> (Last <$> optional (option auto (long "messages"      <> metavar "MSGS")))
  <*> (Last <$> optional (option auto (long "socket"        <> metavar "FILE")))
  <*> (Last <$> optional (option (NetworkMagic <$> auto)
                           (long "network-magic" <> metavar "INT")))

mergeTestSetup :: TestSetup Last -> TestSetup Identity
mergeTestSetup TestSetup{..} =
  TestSetup
  { tsTime         = get "Missing tsTime"         tsTime
  , tsThreads      = get "Missing tsThreads"      tsThreads
  , tsMessages     = Identity . join $ getLast           tsMessages
  , tsSocketPath   = get "Missing tsSocketPath"   tsSocketPath
  , tsNetworkMagic = get "Missing tsNetworkMagic" tsNetworkMagic
  }
 where
  get desc = Identity . fromMaybe (error $ "Missing " <> desc) . getLast

getTestSetup :: IO (TestSetup Identity)
getTestSetup =
  customExecParser
    (prefs showHelpOnEmpty)
    (info parseTestSetup mempty)
    <&> (defaultTestSetup <>)
    <&> mergeTestSetup

-- | configuration for testing
simpleTestConfig :: TraceConfig
simpleTestConfig = emptyTraceConfig {
  tcOptions = fromList
    [([] :: Namespace,
         [ ConfSeverity (SeverityF (Just Debug))
         , ConfDetail DNormal
         , ConfBackend [Forwarder]
         ])
    ]
  }

-- | Run scripts in three threads in parallel.
--   The duration of the test is given by time in seconds
runScriptForwarding ::
     TestSetup Identity
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> IORef Int
  -> Property
runScriptForwarding ts@TestSetup{..} fwdTracer stdoutTracer' accumulationCounter =
    trace ("Test setup " ++ show ts) $ do
      let generator :: Gen [Script] = vectorOf (runIdentity tsThreads) $
            case runIdentity tsMessages of
              Nothing -> scale (* 1000) arbitrary
              Just numMsg -> Script <$> vectorOf numMsg arbitrary
      forAll generator (\ (scripts :: [Script])
        -> ioProperty $ do
          tr <- mkCardanoTracer
                      stdoutTracer'
                      fwdTracer
                      Nothing
                      ["Test"]
                      namesForMessage
                      severityForMessage
                      privacyForMessage
          configureTracers simpleTestConfig docMessage [tr]
          let scripts' = map (\ (Script sc) -> Script
                             $ filter (\(ScriptedMessage _ msg) ->
                                namesForMessage msg /= ["Message2"]) sc) scripts
              scripts'' = map (\ (Script sc) -> Script (sort sc))  scripts'
              scripts''' = zipWith (\ (Script sc) ind -> Script (
                            withMessageIds (runIdentity tsThreads) ind sc)) scripts'' [0..]
              scripts'''' = map (\ (Script sc) -> Script
                              $ map (withTimeFactor (runIdentity tsTime)) sc) scripts'''


          -- putStrLn ("runTest " ++ show scripts)
          children :: MVar [MVar (Either SomeException ())] <- newMVar []
          mapM_ (\sc -> forkChild children (playIt sc tr 0.0)) scripts''''
          res <- waitForChildren children []
          let resErr = mapMaybe
                      (\case
                              Right _ -> Nothing
                              Left err -> Just err) res
          threadDelay 500000 --wait 0,5 seconds
          if not (null resErr)
            then throw (head resErr)
            else        -- Oracle
              let numMsg = sum (map (\ (Script sc) -> length sc) scripts'''')
              in if numMsg > 0 then do
                -- TODO mutiple files
                contents <- readFile "/tmp/cardano-forwarder-test-logs/tmp-tracersock@0/node.json"
                let lineLength = length (lines contents) - 1
                putStrLn $ "Line length " ++ show lineLength
                putStrLn $ "Msg length " ++ show numMsg
                totalNumMsg <- atomicModifyIORef accumulationCounter (\ac ->
                  let nc = ac + numMsg
                  in (nc, nc))
                pure (totalNumMsg == lineLength)
              else do
                putStrLn "Empty test"
                pure True

        )

forkChild :: MVar [MVar (Either SomeException ())] -> IO () -> IO ThreadId
forkChild children io = do
   mvar <- newEmptyMVar
   childs <- takeMVar children
   putMVar children (mvar:childs)
   forkFinally io (putMVar mvar)

waitForChildren :: MVar [MVar (Either SomeException ())]
  -> [Either SomeException ()]
  -> IO [Either SomeException ()]
waitForChildren children accum = do
 cs <- takeMVar children
 case cs of
   []   -> pure accum
   m:ms -> do
      putMVar children ms
      res <- takeMVar m
      waitForChildren children (res : accum)


-- | Play the current script in one thread
-- The time is in milliseconds
playIt :: Script -> Trace IO Message -> Double -> IO ()
playIt (Script []) _tr _d = pure ()
playIt (Script (ScriptedMessage d1 m1 : rest)) tr d = do
  when (d < d1) $ threadDelay (round ((d1 - d) * 1000000))
    -- this is in microseconds
  traceWith tr m1
  playIt (Script rest) tr d1

-- | Adds a message id to every message.
-- MessageId gives the id to start with.
-- Returns a tuple with the messages with ids and
-- the successor of the last used messageId
withMessageIds :: Int -> MessageID -> [ScriptedMessage] -> [ScriptedMessage]
withMessageIds numThreads mid sMsgs = go mid sMsgs []
  where
    go _mid' [] acc = reverse acc
    go mid' (ScriptedMessage time msg : tl) acc =
      go (mid' + numThreads) tl (ScriptedMessage time (setMessageID msg mid') : acc)

withTimeFactor :: Double -> ScriptedMessage -> ScriptedMessage
withTimeFactor factor (ScriptedMessage time msg) =
    ScriptedMessage (time * factor) msg
