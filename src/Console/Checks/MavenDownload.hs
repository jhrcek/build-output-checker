{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Console.Checks.MavenDownload
  ( getMavenDownloadDurations
  , getDistinctDownloadUrls
  ) where

import Console.Types (Duration (..), LogLine (..), TimedLogLine (..),
                      TransferType (Download), diffElapsed, getInterval,
                      getLogLine)
import Data.List.Split (wordsBy)
import Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)

getMavenDownloadDurations :: [TimedLogLine] -> [Duration]
getMavenDownloadDurations =
    fmap (uncurry diffElapsed . getInterval) . wordsBy (not . lineWithMavenDownload . getLogLine)
  where
      lineWithMavenDownload :: LogLine -> Bool
      lineWithMavenDownload = \case
          MavenTransferStart Download _ _   -> True
          MavenTransferEnd Download _ _ _ _ -> True
          _                                 -> False

getDistinctDownloadUrls :: [LogLine] -> Set Text
getDistinctDownloadUrls = foldMap toUrlSet
  where
    toUrlSet = \case
        MavenTransferStart Download _ url   -> Set.singleton url
        MavenTransferEnd Download _ url _ _ -> Set.singleton url
        _                                   -> Set.empty
