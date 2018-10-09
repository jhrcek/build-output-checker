{-# LANGUAGE OverloadedStrings #-}
module Main where

import Console.Checks.MavenDownload (getDistinctDownloadUrls,
                                     getMavenDownloadDurations)
import Console.Checks.MavenPlugin (pluginDurations)
import Console.Parse (parseTimestamps)
import Console.Types (diffElapsed, getInterval, getLogLine)
import qualified Data.Set as Set
import qualified Data.Text.IO as T

main :: IO ()
main = do
    log_ts <- parseTimestamps =<< T.readFile "consoleText_timestamps"
    putStrLn $ "Total duration: " <> show (uncurry diffElapsed $ getInterval log_ts)
    putStrLn $ "Time spent downloading: " <> show (mconcat $ getMavenDownloadDurations log_ts)
    let urls = getDistinctDownloadUrls $ fmap getLogLine log_ts
    putStrLn $ "Number of distinct download URLs: " <> show (Set.size urls)
    mapM_ print . take 10 . reverse $ pluginDurations log_ts
