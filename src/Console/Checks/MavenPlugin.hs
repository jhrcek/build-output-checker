{-# LANGUAGE LambdaCase #-}
module Console.Checks.MavenPlugin (pluginDurations) where
import Console.Types
import Data.List (sortOn)

pluginDurations :: [TimedLogLine] -> [(PluginExecution, Duration)]
pluginDurations linez =
    sortOn (\(_,duration) -> duration) $ zipWith calcDuration pluginStarts (getElapsedTime <$> tail pluginStarts)
  where
    pluginStarts = filter isMavenPluginExecution linez

calcDuration :: TimedLogLine -> ElapsedTime -> (PluginExecution, Duration)
calcDuration (TimedLogLine tsStart (MavenPluginExecution ple)) timestampOfNextPluginStart
    = (ple, diffElapsed tsStart timestampOfNextPluginStart)

isMavenPluginExecution :: TimedLogLine -> Bool
isMavenPluginExecution = \case
    (TimedLogLine _ (MavenPluginExecution _)) -> True
    _ -> False
