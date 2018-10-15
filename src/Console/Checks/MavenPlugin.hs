{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Console.Checks.MavenPlugin (getPluginStats, PluginStats(..)) where

import Console.Types
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)

data PluginStats = PluginStats
    { pluginDurations   :: [(PluginExecution, Duration)]
    , durationPerPlugin :: [(Text, Duration)]
    }

getPluginStats :: [TimedLogLine] -> PluginStats
getPluginStats linez = PluginStats {..}
  where
    allPluginDurations = getPluginDurations linez
    pluginDurations = takeSlowerThan (secondsToDuration 60) allPluginDurations
    durationPerPlugin = sortByDurationDescending . Map.toList $
        Map.fromListWith (<>) [(pluginName pe, dur) | (pe, dur) <- allPluginDurations]

getPluginDurations :: [TimedLogLine] -> [(PluginExecution, Duration)]
getPluginDurations linez =
     sortByDurationDescending $ zipWith calcDuration pluginStarts (tail pluginStarts)
  where
    pluginStarts = mapMaybe getPluginExecution linez

sortByDurationDescending :: [(a, Duration)] -> [(a, Duration)]
sortByDurationDescending = sortOn (Down . snd)

takeSlowerThan :: Duration -> [(a, Duration)] -> [(a,Duration)]
takeSlowerThan threshold =
    takeWhile (\(_, duration) -> duration > threshold)

calcDuration :: (PluginExecution, ElapsedTime)
             -> (PluginExecution, ElapsedTime)
             -> (PluginExecution, Duration)
calcDuration (pl1, start1) (_, start2)
    = (pl1, diffElapsed start1 start2)

getPluginExecution :: TimedLogLine -> Maybe (PluginExecution, ElapsedTime)
getPluginExecution = \case
    (TimedLogLine et (MavenPluginExecution ple)) -> Just (ple, et)
    _                                            -> Nothing
