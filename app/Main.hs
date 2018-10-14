{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Console.Checks.MavenDownload (MavenData (..), getDownloadSumary,
                                     getTimeSpentDownloading)
import Console.Checks.MavenPlugin (pluginDurations)
import Console.Checks.TestDuration (getClassInfos, mdDuration,
                                    readMethodDurations)
import Console.Parse (parseTimestamps)
import Console.Report (ReportData (..), writeReport)
import Console.Types (diffElapsed, getInterval, getLogLine, secondsToDuration,
                      tciTimeElapsed)
import qualified Data.Text.IO as T

main :: IO ()
main = do
    log_ts <- parseTimestamps =<< T.readFile "consoleText_timestamps"
    testInfos <- readMethodDurations "junitreport"
    let log_plain = getLogLine <$> log_ts
        buildDuration = uncurry diffElapsed $ getInterval log_ts
        slowTestMethods = takeWhile (\methodInfo -> mdDuration methodInfo > 20) testInfos
        slowTestClasses = takeWhile (\classInfo -> tciTimeElapsed classInfo > 60) $ getClassInfos log_plain
        (urlsDownloaded, totalDownloadSize) = getDownloadSumary log_plain
        slowPlugins = takeWhile (\(_pluginEx, duration) -> duration > secondsToDuration 60) $ pluginDurations log_ts
        mavenData = MavenData
            { urlsDownloaded = urlsDownloaded
            , totalDownloadSize = totalDownloadSize
            , timeSpentDownloading = getTimeSpentDownloading log_ts
            }
    let reportData = ReportData {..}
    writeReport reportData "report.html"
