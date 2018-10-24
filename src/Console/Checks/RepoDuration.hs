{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Console.Checks.RepoDuration (getBuildDurationPerRepo) where

import Console.Checks.MavenPlugin (sortByDurationDescending)
import Console.Types
import Data.List.Split (dropBlanks, keepDelimsL, split, whenElt)
import qualified Data.Map as Map

getBuildDurationPerRepo :: [TimedLogLine] -> [(GitRepoName, Duration)]
getBuildDurationPerRepo =
     sortByDurationDescending
    . discardNonBuildMavenRuns
    . fmap processRepoLines
    . splitIntoRepoGroups
    . dropLinesUntilFirstRepoActionStart
  where
    processRepoLines :: [TimedLogLine] -> (GitRepoName, Duration)
    processRepoLines = \case
        linez@(TimedLogLine _ (RepoActionStart repoName):_) ->
            ( repoName
            , getDuration $ takeWhile (not . isRepoBuildEnd) linez
            )
        (otherStartLine:_) -> error $ "Unexpected start of repo line group (was expecting RepoActionStart): " ++ show otherStartLine
        []                 -> error "There were no lines in repo line group!"

    dropLinesUntilFirstRepoActionStart :: [TimedLogLine] -> [TimedLogLine]
    dropLinesUntilFirstRepoActionStart = dropWhile (not . isRepoActionStart)

    splitIntoRepoGroups :: [TimedLogLine] -> [[TimedLogLine]]
    splitIntoRepoGroups = split . dropBlanks . keepDelimsL $ whenElt isRepoActionStart

    -- There are multiple maven runs in the kie-all build (setting versions.
    --Only the longest run (the actual build) is of interest to us
    discardNonBuildMavenRuns :: [(GitRepoName, Duration)] -> [(GitRepoName, Duration)]
    discardNonBuildMavenRuns =
        Map.toList . Map.fromListWith max


isRepoActionStart :: TimedLogLine -> Bool
isRepoActionStart (TimedLogLine _ (RepoActionStart _)) = True
isRepoActionStart _                                    = False

isRepoBuildEnd :: TimedLogLine -> Bool
isRepoBuildEnd (TimedLogLine _ (Maven INFO "BUILD SUCCESS")) = True
isRepoBuildEnd _                                             = False
