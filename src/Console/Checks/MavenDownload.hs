{-# LANGUAGE LambdaCase #-}

module Console.Checks.MavenDownload
  ( MavenData(..)
  , getTimeSpentDownloading
  , getDownloadSumary
  ) where

import Console.Types (Duration (..), FileSize, LogLine (..), TimedLogLine (..),
                      TransferType (Download), diffElapsed, getInterval,
                      getLogLine)
import Data.List.Split (wordsBy)
import Data.Set as Set

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
          MavenTransferStart Download _ _   -> True
          MavenTransferEnd Download _ _ _ _ -> True
          _                                 -> False

getDownloadSumary :: [LogLine] -> (Int, FileSize)
getDownloadSumary linez = (Set.size uniqueUrls, totDownloadSize)
  where
    (uniqueUrls, totDownloadSize) = foldMap toUrlAndSize linez
    toUrlAndSize = \case
        MavenTransferEnd Download _ url fileSize _ -> (Set.singleton url, fileSize)
        _                                          -> mempty
