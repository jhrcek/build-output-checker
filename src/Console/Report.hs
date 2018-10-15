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
import Prelude hiding (div)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, body, details, div, docTypeHtml, meta, string,
                         summary, table, td, text, th, tr, (!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (summary)


data ReportData = ReportData
    { slowTestClasses :: [TestClassInfo]
    , slowTestMethods :: [MethodDuration]
    , mavenData       :: !MavenData
    , buildDuration   :: !Duration
    , pluginStats     :: !PluginStats
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

pluginStatsView :: PluginStats -> Html
pluginStatsView PluginStats{pluginDurations, durationPerPlugin} = div $ do
    slowPluginsView pluginDurations
    summedUpDurationsView durationPerPlugin

slowPluginsView :: [(PluginExecution, Duration)] -> Html
slowPluginsView pluginDurations = details $ do
    summary $ text "Slowest plugin executions"
    table $ do
        tr $ traverse_ th ["Plugin", "Version", "Goal", "Execution", "Maven module", "Duration (s)"]
        mapM_ slowPluginRow pluginDurations

summedUpDurationsView :: [(Text, Duration)] -> Html
summedUpDurationsView summedUpDurations = details $ do
    summary $ text "Total duration per plugin"
    table $ do
        tr $ th "Plugin" >> th "Duration (s)"
        mapM_ pluginSummary summedUpDurations
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
        mapM_ methodRow tcs

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
        mapM_ classRow tcs

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

sh :: Show a => a -> Html
sh = string . show
