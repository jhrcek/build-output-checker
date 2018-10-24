{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Console.Checks.MavenDownload (getMavenDownloadData)
import Console.Checks.MavenPlugin (getPluginStats)
import Console.Checks.TestDuration (getClassInfos, mdDuration,
                                    readMethodDurations)
import Console.Parse (parseTimestamps)
import Console.Report (ReportData (..), writeReport)
import Console.Types (diffElapsed, getInterval, getLogLine, tciTimeElapsed)
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)

main :: IO ()
main = do
    log_ts <- parseTimestamps =<< T.readFile "consoleText_timestamps"
    testInfos <- readMethodDurations "junitreport"
    generatedOn <- getCurrentTime
    let log_plain = getLogLine <$> log_ts
        buildDuration = uncurry diffElapsed $ getInterval log_ts
        slowTestMethods = takeWhile (\methodInfo -> mdDuration methodInfo > 20) testInfos
        slowTestClasses = takeWhile (\classInfo -> tciTimeElapsed classInfo > 60) $ getClassInfos log_plain
        pluginStats = getPluginStats log_ts
        mavenData = getMavenDownloadData log_ts
        reportData = ReportData
            slowTestClasses
            slowTestMethods
            mavenData
            buildDuration
            pluginStats
            generatedOn
    writeReport reportData "report.html"
