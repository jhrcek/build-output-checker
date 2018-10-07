{-# LANGUAGE OverloadedStrings #-}
module Console.Types
  ( RawLine(..)
  , TimedLogLine(..)
  , ElapsedTime(..)
  , Duration(..)
  , LogLine(..)
  , TransferType(..)
  , TransferSpeed(..)
  , FileSize(..)
  , SizeUnit(..)
  , diffElapsed
  , getElapsedTime
  , mkElapsedTime
  , getInterval
  , getLogLine
  ) where

import Data.Text (Text)
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)

newtype RawLine = RawLine Text deriving (Show)

data LogLine
    = MavenTransferStart TransferType RepoName RepoUrl
    | MavenTransferEnd TransferType RepoName RepoUrl FileSize (Maybe TransferSpeed)
    | MavenPluginStart Text -- TODO refine
    | Maven LogLevel Text
    | Unknown
    deriving (Show, Eq)

data LogLevel = INFO | WARNING | ERROR deriving (Show, Eq)

type RepoName = Text
type RepoUrl = Text
data FileSize = FileSize Double SizeUnit deriving (Show, Eq)
data TransferType = Upload | Download deriving (Show, Eq)
data TransferSpeed = TransferSpeed Double SizeUnit deriving (Show, Eq)
data SizeUnit = B | KB | MB deriving (Show, Eq)

data TimedLogLine = TimedLogLine ElapsedTime LogLine deriving (Show)

-- Time elapsed from the time the build started
newtype ElapsedTime = ElapsedTime DiffTime deriving (Show)

-- Duration in seconds
newtype Duration = Duration DiffTime deriving (Show)

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
