{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Console.Report (writeReport, ReportData(..)) where

import Console.Checks.MavenDownload
import Console.Checks.TestDuration
import Console.Types
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Text
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (summary)

data ReportData = ReportData
    { slowTestClasses :: [TestClassInfo]
    , slowTestMethods :: [MethodDuration]
    , mavenData       :: !MavenData
    , buildDuration   :: !Duration
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

summaryView :: Duration -> Html
summaryView buildDuration =
    H.div . text . Text.pack $ "The build took " <> show buildDuration

slowTestMethodsView :: [MethodDuration] -> Html
slowTestMethodsView tcs = details $ do
    summary $ text "Slow test methods"
    table $ do
        tr (th "Class name" >> th "Method Name" >> th "Duration (s)")
        mapM_ methodRow tcs

methodRow :: MethodDuration -> Html
methodRow m = tr $ do
    td (text $ mdClass m)
    td (text $ mdMethod m)
    td (sh $ mdDuration m)

slowTestClassesView :: [TestClassInfo] -> Html
slowTestClassesView tcs = details $ do
    summary $ text "Slow test classes"
    table $ do
        tr (th "Class Name" >> th "Duration (s)" >> th "Methods")
        mapM_ classRow tcs

classRow :: TestClassInfo -> Html
classRow tci = tr $ do
  td (text $ tciFqn tci)
  td (sh $ tciTimeElapsed tci)
  td (sh $ tciRun tci)

mavenDataView :: MavenData -> Html
mavenDataView md =
  H.div . text . Text.pack $
      "Maven spent " <> show (timeSpentDownloading md) <>
      " downloading " <> show (urlsDownloaded md) <>
      " artifacts, total size  " <> show (totalDownloadSize md)

sh :: Show a => a -> Html
sh = string . show
