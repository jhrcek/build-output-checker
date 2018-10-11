{-# LANGUAGE LambdaCase #-}
module Console.Checks.MavenPlugin (pluginDurations) where
import Console.Types
import Data.List (sortOn)
import Data.Maybe (mapMaybe)

pluginDurations :: [TimedLogLine] -> [(PluginExecution, Duration)]
pluginDurations linez =
    sortOn (\(_,duration) -> duration) $ zipWith calcDuration pluginStarts (tail pluginStarts)
  where
    pluginStarts = mapMaybe getPluginExecution linez

calcDuration :: (ElapsedTime, PluginExecution) -> (ElapsedTime, PluginExecution) -> (PluginExecution, Duration)
calcDuration (start1, pl1) (start2, _)
    = (pl1, diffElapsed start1 start2)

getPluginExecution :: TimedLogLine -> Maybe (ElapsedTime, PluginExecution)
getPluginExecution = \case
    (TimedLogLine et (MavenPluginExecution ple)) -> Just (et, ple)
    _                                            -> Nothing
