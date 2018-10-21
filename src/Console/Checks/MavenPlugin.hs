{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Console.Checks.MavenPlugin (getPluginStats, PluginStats(..)) where

import Console.Types
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set

data PluginStats = PluginStats
    { pluginDurations             :: [(PluginExecution, Duration)]
    , durationPerPlugin           :: [(PluginName, Duration)]
    , pluginsWithMultipleVersions :: [(PluginName, [PluginVersion])]
    }

getPluginStats :: [TimedLogLine] -> PluginStats
getPluginStats linez = PluginStats {..}
  where
    pluginStartLines = getPluginStartLines linez

    allPluginDurations :: [(PluginExecution, Duration)]
    allPluginDurations = getPluginDurations pluginStartLines

    pluginDurations :: [(PluginExecution, Duration)]
    pluginDurations = takeSlowerThan (secondsToDuration 60) allPluginDurations

    durationPerPlugin :: [(PluginName, Duration)]
    durationPerPlugin = sortByDurationDescending . Map.toList $
        Map.fromListWith (<>) [(pluginName pe, dur) | (pe, dur) <- allPluginDurations]

    pluginsWithMultipleVersions :: [(PluginName, [PluginVersion])]
    pluginsWithMultipleVersions =
        Map.toList
        $ fmap Set.toList
        $ Map.filter (\versions -> Set.size versions > 1)
        $ Map.fromListWith Set.union [(pluginName pe, Set.singleton (pluginVersion pe)) | (pe, _) <- pluginStartLines]

getPluginDurations :: [(PluginExecution, ElapsedTime)] -> [(PluginExecution, Duration)]
getPluginDurations pluginStarts =
     sortByDurationDescending $ zipWith calcDuration pluginStarts (tail pluginStarts)

getPluginStartLines :: [TimedLogLine] -> [(PluginExecution, ElapsedTime)]
getPluginStartLines =
    mapMaybe getPluginExecution

sortByDurationDescending :: [(a, Duration)] -> [(a, Duration)]
sortByDurationDescending = sortOn (Down . snd)

takeSlowerThan :: Duration -> [(a, Duration)] -> [(a, Duration)]
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
