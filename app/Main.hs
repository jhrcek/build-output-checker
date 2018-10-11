{-# LANGUAGE OverloadedStrings #-}
module Main where

import Console.Checks.MavenDownload (getDistinctDownloadUrls,
                                     getMavenDownloadDurations)
import Console.Checks.MavenPlugin (pluginDurations)
import Console.Checks.TestDuration (getClassInfos, readMethodDurations,
                                    tiDuration)
import Console.Parse (parseTimestamps)
import Console.Types (diffElapsed, getInterval, getLogLine, tciTimeElapsed)
import qualified Data.Set as Set
import qualified Data.Text.IO as T

main :: IO ()
main = do
    log_ts <- parseTimestamps =<< T.readFile "consoleText_timestamps"
    let log_plain = getLogLine <$> log_ts

    section
    putStrLn $ "Total duration: " <> show (uncurry diffElapsed $ getInterval log_ts)
    section
    putStrLn $ "Time spent downloading: " <> show (mconcat $ getMavenDownloadDurations log_ts)

    let urls = getDistinctDownloadUrls log_plain
    putStrLn $ "Number of distinct download URLs: " <> show (Set.size urls)
    mapM_ print . take 10 . reverse $ pluginDurations log_ts

    testInfos <- readMethodDurations "junitreport"
    let testsOver10Seconds = length . takeWhile (\testInfo -> tiDuration testInfo > 10) $ testInfos
    section
    putStrLn $ "Number of test methods that take over 10 seconds to run: " <> show testsOver10Seconds
    let classesOverMinute = takeWhile (\classInfo -> tciTimeElapsed classInfo > 60) $ getClassInfos log_plain
    mapM_ print classesOverMinute

section :: IO ()
section = putStrLn (replicate 80 '-')
