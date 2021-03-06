{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Console.Report (writeReport, ReportData(..)) where

import Console.Checks.MavenDownload (MavenData, timeSpentDownloading,
                                     totalDownloadSize, urlsDownloaded)
import Console.Checks.MavenPlugin (PluginStats (..))
import Console.Checks.TestDuration (MethodDuration (..), mdClass, mdDuration,
                                    mdMethod)
import Console.Types
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Prelude hiding (div)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, a, body, details, div, docTypeHtml, footer, meta,
                         string, summary, table, td, text, th, tr, (!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (summary)

data ReportData = ReportData
    { slowTestClasses :: [TestClassInfo]
    , slowTestMethods :: [MethodDuration]
    , mavenData       :: !MavenData
    , buildDuration   :: !Duration
    , pluginStats     :: !PluginStats
    , durationPerRepo :: ![(GitRepoName, Duration)]
    , generatedOn     :: !UTCTime
    , buildFinishedOn :: Maybe Text
    }

writeReport :: ReportData -> FilePath -> IO ()
writeReport reportData reportFile =
    Text.writeFile reportFile
        (renderHtml $ reportView reportData)

reportView :: ReportData -> Html
reportView ReportData{..} =
    docTypeHtml $ do
        H.head $ do
            meta ! charset "UTF-8"
            H.title "kieAllBuild summary"
        body $ do
            summaryView buildDuration
            mavenDataView mavenData
            slowTestClassesView slowTestClasses
            slowTestMethodsView slowTestMethods
            pluginStatsView pluginStats
            durationPerRepoView durationPerRepo
            infoFooter generatedOn buildFinishedOn

durationPerRepoView :: [(GitRepoName, Duration)] -> Html
durationPerRepoView xs = details $ do
    summary $ text "Build time per repository"
    table $ do
        tr $ th "Repository" >> th "Build time (HH:MM:SS)"
        traverse_ repoRow xs
  where
    repoRow (GitRepoName repNm, duration) = tr $ do
        td $ text repNm
        td $ sh duration

pluginStatsView :: PluginStats -> Html
pluginStatsView PluginStats{pluginDurations, durationPerPlugin, pluginsWithMultipleVersions} = div $ do
    slowPluginsView pluginDurations
    summedUpDurationsView durationPerPlugin
    pluginsWithMultipleVersionsView pluginsWithMultipleVersions

pluginsWithMultipleVersionsView :: [(PluginName, [PluginVersion])] -> Html
pluginsWithMultipleVersionsView xs = details $ do
    summary $ text "Plugins with multiple versions"
    table $ do
        tr $ th "Plugin" >> th "Versions"
        traverse_ pluginRow xs
    where
      pluginRow (plugName, versionList) = tr $ do
          td $ text plugName
          td $ sh versionList

slowPluginsView :: [(PluginExecution, Duration)] -> Html
slowPluginsView pluginDurations = details $ do
    summary $ text "Slowest plugin executions"
    table $ do
        tr $ traverse_ th ["Plugin", "Version", "Goal", "Execution", "Maven module", "Duration (HH:MM:SS)"]
        traverse_ slowPluginRow pluginDurations

summedUpDurationsView :: [(Text, Duration)] -> Html
summedUpDurationsView summedUpDurations = details $ do
    summary $ text "Total duration per plugin"
    table $ do
        tr $ th "Plugin" >> th "Duration (HH:MM:SS)"
        traverse_ pluginSummary summedUpDurations
    where
      pluginSummary (plName, duration) = tr $ do
        td $ text plName
        td $ sh duration

slowPluginRow :: (PluginExecution, Duration) -> Html
slowPluginRow (PluginExecution{..}, duration) = tr $ do
    td $ text pluginName
    td $ text pluginVersion
    td $ text pluginGoal
    td $ text pluginExecutionId
    td $ text pluginMavenModue
    td $ sh duration

summaryView :: Duration -> Html
summaryView buildDuration =
    div . text . Text.pack $ "The build took " <> show buildDuration

slowTestMethodsView :: [MethodDuration] -> Html
slowTestMethodsView tcs = details $ do
    summary $ text "Slow test methods"
    table $ do
        tr (th "Class name" >> th "Method Name" >> th "Duration (s)")
        traverse_ methodRow tcs

methodRow :: MethodDuration -> Html
methodRow MethodDuration{..} = tr $ do
    td $ text mdClass
    td $ text mdMethod
    td $ sh mdDuration

slowTestClassesView :: [TestClassInfo] -> Html
slowTestClassesView tcs = details $ do
    summary $ text "Slow test classes"
    table $ do
        tr (th "Class Name" >> th "Duration (s)" >> th "Methods")
        traverse_ classRow tcs

classRow :: TestClassInfo -> Html
classRow TestClassInfo{..} = tr $ do
    td $ text tciFqn
    td $ sh tciTimeElapsed
    td $ sh tciRun

mavenDataView :: MavenData -> Html
mavenDataView md =
  div . text . Text.pack $
      "Maven spent " <> show (timeSpentDownloading md) <>
      " downloading " <> show (urlsDownloaded md) <>
      " artifacts, total size  " <> show (totalDownloadSize md)

infoFooter :: UTCTime -> Maybe Text -> Html
infoFooter generatedOn mBuildFinished =
    footer ! style "position:fixed;left:0;bottom:0;width:100%;text-align:center;" $ do
        div $ do
            text "Generated by "
            a ! href "https://github.com/jhrcek/build-output-checker" $ text "build-output-checker"
            text " on "
            string $ formatTimestamp generatedOn
        div finishedInfo
  where
    finishedInfo = case mBuildFinished of
      Just timestamp -> text $ "based on a build that finished on " <> timestamp
      Nothing -> do
          text "Failed to determine when the build, that this report is based on, finished. File an "
          a ! href "https://github.com/jhrcek/build-output-checker/issues" $ text "issue"
          text " if you see this!"


sh :: Show a => a -> Html
sh = string . show

formatTimestamp :: UTCTime -> String
formatTimestamp =
    formatTime defaultTimeLocale "%Y-%m-%d at %H:%M:%S"
