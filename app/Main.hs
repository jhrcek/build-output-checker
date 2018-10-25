{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Console.Checks.BuildFinished (getBuildFinishedTimeStamp)
import Console.Checks.MavenDownload (getMavenDownloadData)
import Console.Checks.MavenPlugin (getPluginStats)
import Console.Checks.RepoDuration (getBuildDurationPerRepo)
import Console.Checks.TestDuration (getClassInfos, mdDuration,
                                    readMethodDurations)
import Console.Parse (parseTimestamps)
import Console.Report (ReportData (..), writeReport)
import Console.Types (getDuration, getLogLine, tciTimeElapsed)
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)

main :: IO ()
main = do
    log_ts <- parseTimestamps =<< T.readFile "consoleText_timestamps"
    testInfos <- readMethodDurations "junitreport"
    generatedOn <- getCurrentTime
    let log_plain = getLogLine <$> log_ts
        -- ReportData fields
        slowTestClasses = takeWhile (\classInfo -> tciTimeElapsed classInfo > 60) $ getClassInfos log_plain
        slowTestMethods = takeWhile (\methodInfo -> mdDuration methodInfo > 20) testInfos
        mavenData = getMavenDownloadData log_ts
        buildDuration = getDuration log_ts
        pluginStats = getPluginStats log_ts
        durationPerRepo = getBuildDurationPerRepo log_ts
        buildFinishedOn = getBuildFinishedTimeStamp log_plain
        reportData = ReportData{..}
    writeReport reportData "report.html"
