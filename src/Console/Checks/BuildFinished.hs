{-# LANGUAGE OverloadedStrings #-}

module Console.Checks.BuildFinished (getBuildFinishedTimeInfo) where
import Console.Types
import Data.Maybe (mapMaybe)
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text

getBuildFinishedTimeInfo :: [LogLine] -> Text
getBuildFinishedTimeInfo linez = case mapMaybe extractBuildFinishedTimestamp linez of
    [] -> "Failed to determine when the build finished"
    -- Take the last timestamps (there is one for each maven build);
    -- The timestamp is like "2018-10-24 07:48:37-04:00" so take only date and time
    timestamps -> "Build finished at " <> Text.take 19 (Text.replace "T" " " (last timestamps))


extractBuildFinishedTimestamp :: LogLine -> Maybe Text
extractBuildFinishedTimestamp (Maven INFO line)  | "Finished at: " `isPrefixOf` line =
    Just $ Text.drop (Text.length "Finished at: ") line
extractBuildFinishedTimestamp _ = Nothing
