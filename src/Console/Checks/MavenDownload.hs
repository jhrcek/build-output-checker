{-# LANGUAGE OverloadedStrings #-}
module Console.Checks.MavenDownload
  ( getMavenDownloadDurations
  ) where

import Console.Types (Duration (..), LogLine (..),
                      TimedLogLine (..), diffElapsed,
                      getInterval)
import Data.List.Split (wordsBy)
import qualified Data.Text as T

getMavenDownloadDurations :: [TimedLogLine] -> [Duration]
getMavenDownloadDurations =
    fmap (uncurry diffElapsed . getInterval) . wordsBy (not . lineWithMavenDownload)
  where
      lineWithMavenDownload :: TimedLogLine -> Bool
      lineWithMavenDownload (TimedLogLine _ (LogLine x)) =
          T.isInfixOf "[INFO] Downloading from" x ||
          T.isInfixOf "[INFO] Downloaded from"  x
