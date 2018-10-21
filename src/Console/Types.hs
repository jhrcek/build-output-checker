{-# LANGUAGE OverloadedStrings #-}
module Console.Types
  ( Duration(..)
  , ElapsedTime(..)
  , FileSize(..)
  , LogLine(..)
  , MavenTransfer(..)
  , PluginExecution(..)
  , PluginName
  , PluginVersion
  , RawLine(..)
  , RepoName(..)
  , RepoUrl(..)
  , SizeUnit(..)
  , TestClassInfo(..)
  , TimedLogLine(..)
  , TransferSpeed(..)
  , TransferStartOrEnd(..)
  , TransferType(..)
  , diffElapsed
  , getElapsedTime
  , mkElapsedTime
  , getInterval
  , getLogLine
  , secondsToDuration
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (timeToTimeOfDay)
import Text.Printf (printf)
newtype RawLine = RawLine Text deriving (Show)

data LogLine
    = MavenTransferLine !MavenTransfer
    | PluginExecutionLine !PluginExecution
    | TestClassInfoLine !TestClassInfo
    | Maven !LogLevel !Text
    | Unknown
    deriving (Show, Eq)

data MavenTransfer = MavenTransfer
    { transferType       :: !TransferType
    , repoName           :: !RepoName
    , repoUrl            :: !RepoUrl
    , transferStartOrEnd :: TransferStartOrEnd
    } deriving (Eq, Show)

data PluginExecution = PluginExecution
    { pluginName        :: !PluginName
    , pluginVersion     :: !PluginVersion
    , pluginGoal        :: !Text
    , pluginExecutionId :: !Text
    , pluginMavenModue  :: !Text
    } deriving Eq

data TestClassInfo = TestClassInfo
  { tciRun         :: !Int
  , tciFailures    :: !Int
  , tciErrors      :: !Int
  , tciSkipped     :: !Int
  , tciTimeElapsed :: !Double
  , tciFqn         :: !Text
  } deriving (Show, Eq)

instance Show PluginExecution where
  show (PluginExecution name vers goal exec modul) =
      Text.unpack $ name <> ":" <> vers <> ":" <> goal <> " ("<> exec <>") @ " <> modul

data LogLevel = INFO | WARNING | ERROR deriving (Show, Eq)

newtype RepoName = RepoName Text deriving (Eq, Show, Ord)
newtype RepoUrl = RepoUrl Text deriving (Eq, Show, Ord)
type PluginName = Text
type PluginVersion = Text

-- Time elapsed from the time the build started
newtype ElapsedTime = ElapsedTime DiffTime deriving (Show)
-- Duration in seconds
newtype Duration = Duration DiffTime deriving (Eq, Ord)

-- Stuff related to maven download / upload info
data TransferType = Upload | Download deriving (Show, Eq, Ord)
data TransferSpeed = TransferSpeed Double SizeUnit deriving (Show, Eq)
data TransferStartOrEnd
    = TransferStart
    | TransferEnd !FileSize !(Maybe TransferSpeed)
    deriving (Show, Eq)
data FileSize = FileSize Double SizeUnit deriving Eq
data SizeUnit = B | KB | MB deriving (Show, Eq)
data TimedLogLine = TimedLogLine ElapsedTime LogLine deriving (Show)


instance Semigroup Duration where
    Duration a <> Duration b = Duration (a + b)

instance Monoid Duration where
    mempty = Duration 0

instance Show Duration where
    show (Duration dt) = formatTime defaultTimeLocale "%T" $ timeToTimeOfDay dt

getBytes :: FileSize -> Double
getBytes (FileSize x u) = case u of
    B  -> x
    KB -> x * 1000
    MB -> x * 1000000

instance Show FileSize where
  show fs
     | bytes > 1000000 = printf "%.1fMB" (bytes / 1000000)
     | bytes > 1000    = printf "%.1fKB" (bytes /1000)
     | otherwise       = printf "%.1fB" bytes
    where
      bytes = getBytes fs

instance Semigroup FileSize where
  a <> b = FileSize (getBytes a + getBytes b) B

instance Monoid FileSize where
  mempty = FileSize 0 B

getLogLine :: TimedLogLine -> LogLine
getLogLine (TimedLogLine _ ll) = ll

getElapsedTime :: TimedLogLine -> ElapsedTime
getElapsedTime (TimedLogLine et _) = et

diffElapsed :: ElapsedTime -> ElapsedTime -> Duration
diffElapsed (ElapsedTime a) (ElapsedTime b) = Duration $ abs (a - b)

-- INPUT: timestamp number parsed from the console log
mkElapsedTime :: Double -> ElapsedTime
mkElapsedTime =
    ElapsedTime . picosecondsToDiffTime .  round . (*10^(12::Int))

secondsToDuration :: Double -> Duration
secondsToDuration =
    Duration . picosecondsToDiffTime .  round . (*10^(12::Int))

{-|  Get first and last timestamp from a list of log lines.
 WARNING: This assumes the lines are sorted by timestamp!
-}
getInterval :: [TimedLogLine] -> (ElapsedTime, ElapsedTime)
getInterval xs =
    ( getElapsedTime (head xs)
    , getElapsedTime (last xs)
    )
