{-# LANGUAGE OverloadedStrings #-}
module Console.Types
  ( Duration(..)
  , ElapsedTime(..)
  , FileSize(..)
  , LogLine(..)
  , PluginExecution(..)
  , RawLine(..)
  , RepoName(..)
  , RepoUrl(..)
  , SizeUnit(..)
  , TimedLogLine(..)
  , TransferSpeed(..)
  , TransferType(..)
  , diffElapsed
  , getElapsedTime
  , mkElapsedTime
  , getInterval
  , getLogLine
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)

newtype RawLine = RawLine Text deriving (Show)

data LogLine
    = MavenTransferStart !TransferType !RepoName !RepoUrl
    | MavenTransferEnd !TransferType !RepoName !RepoUrl !FileSize !(Maybe TransferSpeed)
    | MavenPluginExecution !PluginExecution
    | Maven !LogLevel !Text
    | Unknown
    deriving (Show, Eq)


data PluginExecution = PluginExecution
    { pluginGroupId     :: !Text
    , pluginArtifactId  :: !Text
    , pluginVersion     :: !Text
    , pluginExecutionId :: !Text
    , pluginMavenModue  :: !Text
    } deriving Eq

instance Show PluginExecution where
  show (PluginExecution g a v e m) =
      Text.unpack $ g <> ":" <> a <> ":" <> v <> " ("<> e <>") @ " <> m

data LogLevel = INFO | WARNING | ERROR deriving (Show, Eq)

newtype RepoName = RepoName Text deriving (Eq, Show)
newtype RepoUrl = RepoUrl Text deriving (Eq, Show, Ord)

-- Time elapsed from the time the build started
newtype ElapsedTime = ElapsedTime DiffTime deriving (Show)
-- Duration in seconds
newtype Duration = Duration DiffTime deriving (Eq, Ord, Show)
data FileSize = FileSize Double SizeUnit deriving (Show, Eq)
data TransferType = Upload | Download deriving (Show, Eq)
data TransferSpeed = TransferSpeed Double SizeUnit deriving (Show, Eq)
data SizeUnit = B | KB | MB deriving (Show, Eq)
data TimedLogLine = TimedLogLine ElapsedTime LogLine deriving (Show)

instance Semigroup Duration where
    Duration a <> Duration b = Duration (a + b)
instance Monoid Duration where
    mempty = Duration 0

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

{-|  Get first and last timestamp from a list of log lines.
 WARNING: This assumes the lines are sorted by timestamp!
-}
getInterval :: [TimedLogLine] -> (ElapsedTime, ElapsedTime)
getInterval xs =
    ( getElapsedTime (head xs)
    , getElapsedTime (last xs)
    )
