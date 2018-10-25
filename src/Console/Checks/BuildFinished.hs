{-# LANGUAGE OverloadedStrings #-}

module Console.Checks.BuildFinished (getBuildFinishedTimeStamp) where
import Console.Types
import Data.Maybe (mapMaybe)
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text

getBuildFinishedTimeStamp :: [LogLine] -> Maybe Text
getBuildFinishedTimeStamp linez = case mapMaybe extractBuildFinishedTimestamp linez of
    []         -> Nothing
    -- Take the last timestamps (there is one for each maven build);
    -- The timestamp is like "2018-10-24T07:48:37-04:00" so take only date and time
    timestamps -> Just . Text.replace "T" " at " . Text.take 19 $ last timestamps


extractBuildFinishedTimestamp :: LogLine -> Maybe Text
extractBuildFinishedTimestamp (Maven INFO line)  | "Finished at: " `isPrefixOf` line =
    Just $ Text.drop (Text.length "Finished at: ") line
extractBuildFinishedTimestamp _ = Nothing
