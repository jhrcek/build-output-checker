{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Console.Checks.MavenDownload
  ( MavenData(..)
  , getMavenDownloadData
  ) where

import Console.Types (Duration (..), FileSize, LogLine (..), MavenTransfer (..),
                      TimedLogLine (..), TransferStartOrEnd (..),
                      TransferType (..), diffElapsed, getInterval, getLogLine)
import Data.List.Split (wordsBy)
import Data.Set as Set

getMavenDownloadData :: [TimedLogLine] -> MavenData
getMavenDownloadData timedLines = MavenData {..}
  where
    (urlsDownloaded, totalDownloadSize) = getDownloadSummary $ getLogLine <$> timedLines
    timeSpentDownloading = getTimeSpentDownloading timedLines

data MavenData = MavenData
    { urlsDownloaded       :: !Int
    , totalDownloadSize    :: !FileSize
    , timeSpentDownloading :: !Duration
    }

getTimeSpentDownloading :: [TimedLogLine] -> Duration
getTimeSpentDownloading =
    mconcat . fmap (uncurry diffElapsed . getInterval) . wordsBy (not . lineWithMavenDownload . getLogLine)
  where
      lineWithMavenDownload :: LogLine -> Bool
      lineWithMavenDownload = \case
          MavenTransferLine (MavenTransfer Download _ _ _)   -> True
          _                                                  -> False

getDownloadSummary :: [LogLine] -> (Int, FileSize)
getDownloadSummary linez = (Set.size uniqueUrls, totDownloadSize)
  where
    (uniqueUrls, totDownloadSize) = foldMap toUrlAndSize linez
    toUrlAndSize = \case
        MavenTransferLine (MavenTransfer Download _ url (TransferEnd fileSize _)) -> (Set.singleton url, fileSize)
        _                                                                         -> mempty
