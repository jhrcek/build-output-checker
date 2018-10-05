module Console.Types
  ( LogLine(..)
  , TimedLogLine(..)
  , ElapsedTime(..)
  , Duration(..)
  , diffElapsed
  , getElapsedTime
  , mkElapsedTime
  , getInterval
  ) where
import Data.Text (Text)
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)

newtype LogLine = LogLine Text deriving (Show)

data TimedLogLine = TimedLogLine ElapsedTime LogLine deriving (Show)

-- Time elapsed from the time the build started
newtype ElapsedTime = ElapsedTime DiffTime deriving (Show)

-- Duration in seconds
newtype Duration = Duration DiffTime deriving (Show)

instance Semigroup Duration where
    Duration a <> Duration b = Duration (a + b)
instance Monoid Duration where
    mempty = Duration 0

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
