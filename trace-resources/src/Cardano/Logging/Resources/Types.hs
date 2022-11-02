{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Logging.Resources.Types
    ( Resources(..)
    , ResourceStats
    , docResourceStats
    ) where


import           Cardano.Logging
import           Data.Aeson
import           Data.Text (pack)
import           Data.Word
import           GHC.Generics (Generic)

-- | Struct for resources used by the process
type ResourceStats = Resources Word64

-- * HKD for resources used by the process.
--
data Resources a
  = Resources
      { rCentiCpu   :: !a
      , rCentiGC    :: !a
      , rCentiMut   :: !a
      , rGcsMajor   :: !a
      , rGcsMinor   :: !a
      , rAlloc      :: !a
      , rLive       :: !a
      , rHeap       :: !a
      , rRSS        :: !a
      , rCentiBlkIO :: !a
      , rNetBytesRd :: !a
      , rNetBytesWr :: !a
      , rFsBytesRd  :: !a
      , rFsBytesWr  :: !a
      , rThreads    :: !a
      }
  deriving (Functor, Generic, Show)

instance Applicative Resources where
  pure a = Resources a a a a a a a a a a a a a a a
  f <*> x =
    Resources
    { rCentiCpu   = rCentiCpu   f (rCentiCpu   x)
    , rCentiGC    = rCentiGC    f (rCentiGC    x)
    , rCentiMut   = rCentiMut   f (rCentiMut   x)
    , rGcsMajor   = rGcsMajor   f (rGcsMajor   x)
    , rGcsMinor   = rGcsMinor   f (rGcsMinor   x)
    , rAlloc      = rAlloc      f (rAlloc      x)
    , rLive       = rLive       f (rLive       x)
    , rHeap       = rHeap       f (rHeap       x)
    , rRSS        = rRSS        f (rRSS        x)
    , rCentiBlkIO = rCentiBlkIO f (rCentiBlkIO x)
    , rNetBytesRd = rNetBytesRd f (rNetBytesRd x)
    , rNetBytesWr = rNetBytesWr f (rNetBytesWr x)
    , rFsBytesRd  = rFsBytesRd  f (rFsBytesRd  x)
    , rFsBytesWr  = rFsBytesWr  f (rFsBytesWr  x)
    , rThreads    = rThreads    f (rThreads    x)
    }

instance FromJSON a => FromJSON (Resources a) where
  parseJSON = genericParseJSON jsonEncodingOptions

instance ToJSON a => ToJSON (Resources a) where
  toJSON = genericToJSON jsonEncodingOptions
  toEncoding = genericToEncoding jsonEncodingOptions

jsonEncodingOptions :: Options
jsonEncodingOptions = defaultOptions
  { fieldLabelModifier     = drop 1
  , tagSingleConstructors  = True
  , sumEncoding =
    TaggedObject
    { tagFieldName = "kind"
    , contentsFieldName = "contents"
    }
  }

docResourceStats :: Documented ResourceStats
docResourceStats = Documented [
      DocMsg
        []
        [("Resources.Stat.Cputicks", "Kernel-reported CPU ticks (1/100th of a second), since process start")
        ,("Resources.Mem.Resident", "Kernel-reported RSS (resident set size)")
        ,("Resources.RTS.GcLiveBytes", "RTS-reported live bytes")
        ,("Resources.RTS.GcMajorNum", "Major GCs")
        ,("Resources.RTS.GcMinorNum", "Minor GCs")
        ,("Resources.RTS.Gcticks", "RTS-reported CPU ticks spent on GC")
        ,("Resources.RTS.Mutticks", "RTS-reported CPU ticks spent on mutator")
        ,("Resources.State.NetBytesRd", "IP packet bytes read")
        ,("Resources.State.NetBytesWr", "IP packet bytes written")
        ,("Resources.State.FsBytesRd", "FS bytes read")
        ,("Resources.State.FsBytesWr", "FS bytes written")
        ,("Resources.RTS.Threads","RTS green thread count")
        ]
        ""
    ]

instance LogFormatting ResourceStats where
    forHuman Resources{..} = "Resources:"
                  <>  " Cpu Ticks "            <> (pack . show) rCentiCpu
                  <> ", GC centiseconds "      <> (pack . show) rCentiGC
                  <> ", Mutator centiseconds " <> (pack . show) rCentiMut
                  <> ", GCs major "            <> (pack . show) rGcsMajor
                  <> ", GCs minor "            <> (pack . show) rGcsMinor
                  <> ", Allocated bytes "      <> (pack . show) rAlloc
                  <>" , GC live bytes "        <> (pack . show) rLive
                  <> ", RTS heap "             <> (pack . show) rHeap
                  <> ", RSS "                  <> (pack . show) rRSS
                  <> ", Net bytes read "       <> (pack . show) rNetBytesRd
                  <> " written "               <> (pack . show) rNetBytesWr
                  <> ", FS bytes read "        <> (pack . show) rFsBytesRd
                  <> " written "               <> (pack . show) rFsBytesWr
                  <> ", Threads "              <> (pack . show) rThreads
                  <> "."

    forMachine _dtal rs = mconcat
      [ "kind"          .= String "ResourceStats"
      , "CentiCpu"      .= Number (fromIntegral $ rCentiCpu rs)
      , "CentiGC"       .= Number (fromIntegral $ rCentiGC rs)
      , "CentiMut"      .= Number (fromIntegral $ rCentiMut rs)
      , "GcsMajor"      .= Number (fromIntegral $ rGcsMajor rs)
      , "GcsMinor"      .= Number (fromIntegral $ rGcsMinor rs)
      , "Alloc"         .= Number (fromIntegral $ rAlloc rs)
      , "Live"          .= Number (fromIntegral $ rLive rs)
      , "Heap"          .= Number (fromIntegral $ rHeap rs)
      , "RSS"           .= Number (fromIntegral $ rRSS rs)
      , "CentiBlkIO"    .= Number (fromIntegral $ rCentiBlkIO rs)
      , "NetBytesRd"    .= Number (fromIntegral $ rNetBytesRd rs)
      , "NetBytesWr"    .= Number (fromIntegral $ rNetBytesWr rs)
      , "FsBytesRd"     .= Number (fromIntegral $ rFsBytesRd rs)
      , "FsBytesWr"     .= Number (fromIntegral $ rFsBytesWr rs)
      , "Threads"       .= Number (fromIntegral $ rThreads rs)
      ]

    asMetrics rs =
      [ IntM "Resources.Stat.Cputicks"    (fromIntegral $ rCentiCpu rs)
      , IntM "Resources.RTS.Gcticks"      (fromIntegral $ rCentiGC rs)
      , IntM "Resources.RTS.Mutticks"     (fromIntegral $ rCentiMut rs)
      , IntM "Resources.RTS.GcMajorNum"   (fromIntegral $ rGcsMajor rs)
      , IntM "Resources.RTS.GcMinorNum"   (fromIntegral $ rGcsMinor rs)
      , IntM "Resources.RTS.Alloc"        (fromIntegral $ rAlloc rs)
      , IntM "Resources.RTS.GcLiveBytes"  (fromIntegral $ rLive rs)
      , IntM "Resources.RTS.Heap"         (fromIntegral $ rHeap rs)
      , IntM "Resources.Mem.Resident"     (fromIntegral $ rRSS rs)
      , IntM "Resources.Stat.BlkIOticks"  (fromIntegral $ rCentiBlkIO rs)
      , IntM "Resources.State.NetBytesRd" (fromIntegral $ rNetBytesRd rs)
      , IntM "Resources.State.NetBytesWr" (fromIntegral $ rNetBytesWr rs)
      , IntM "Resources.State.FsBytesRd"  (fromIntegral $ rFsBytesRd rs)
      , IntM "Resources.State.FsBytesWr"  (fromIntegral $ rFsBytesWr rs)
      , IntM "Resources.RTS.Stat.Threads" (fromIntegral $ rThreads rs)
      ]
