{-# LANGUAGE LambdaCase #-}
module Console.Checks.MavenPlugin (pluginDurations) where

import Console.Types
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))

pluginDurations :: [TimedLogLine] -> [(PluginExecution, Duration)]
pluginDurations linez =
    sortOn (Down . snd {- duration descending -}) $ zipWith calcDuration pluginStarts (tail pluginStarts)
  where
    pluginStarts = mapMaybe getPluginExecution linez

calcDuration :: (PluginExecution, ElapsedTime)
             -> (PluginExecution, ElapsedTime)
             -> (PluginExecution, Duration)
calcDuration (pl1, start1) (_, start2)
    = (pl1, diffElapsed start1 start2)

getPluginExecution :: TimedLogLine -> Maybe (PluginExecution, ElapsedTime)
getPluginExecution = \case
    (TimedLogLine et (MavenPluginExecution ple)) -> Just (ple, et)
    _                                            -> Nothing
