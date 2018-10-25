{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import Console.Checks.BuildFinished (getBuildFinishedTimeStamp)
import Console.Checks.RepoDuration (getBuildDurationPerRepo)
import Console.Checks.TestDuration (MethodDuration (..))
import Console.Parse (parseLine)
import Console.Types
import qualified Data.Aeson as Aeson
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Console.Parse" $
    describe "parseLine" $ do
      it "should parse lines where maven download starts" $
        parseLine "[INFO] Downloading from mirror-central: http://some.url/1"
          `shouldBe` MavenTransferLine (MavenTransfer Download (M2RepoName "mirror-central") (RepoUrl "http://some.url/1") TransferStart)
      it "should parse lines where maven download ends" $
        parseLine "[INFO] Downloaded from mirror-central: http://some.url/2 (15 B at 30 MB/s)"
          `shouldBe` MavenTransferLine (MavenTransfer Download (M2RepoName "mirror-central") (RepoUrl "http://some.url/2") (TransferEnd (FileSize 15 B) (Just (TransferSpeed 30 MB))))
      it "should parse lines where maven upload starts" $
        parseLine "[INFO] Uploading to local: http://some.url/3"
          `shouldBe` MavenTransferLine (MavenTransfer Upload (M2RepoName "local") (RepoUrl "http://some.url/3") TransferStart)
      it "should parse lines where maven upload ends" $
        parseLine "[INFO] Uploaded to local: http://some.url/4 (368 B at 368 kB/s)"
          `shouldBe` MavenTransferLine (MavenTransfer Upload (M2RepoName "local") (RepoUrl "http://some.url/4") (TransferEnd (FileSize 368 B) (Just (TransferSpeed 368 KB))))
      it "should parse lines where maven upload ends - without transfer speed info" $
        parseLine "[INFO] Uploaded to local: http://some.url/5 (1.9 kB)"
          `shouldBe` MavenTransferLine (MavenTransfer Upload (M2RepoName "local") (RepoUrl "http://some.url/5") (TransferEnd (FileSize 1.9 KB) Nothing))
      it "should parse lines where maven plugin execution starts" $
        parseLine "[INFO] --- buildnumber-maven-plugin:1.4:create (get-scm-revision) @ uberfire-project-client ---"
          `shouldBe` PluginExecutionLine (PluginExecution "buildnumber-maven-plugin" "1.4" "create" "get-scm-revision" "uberfire-project-client")
      it "should parse lines with test class info" $
        parseLine "Tests run: 1, Failures: 2, Errors: 3, Skipped: 4, Time elapsed: 5.694 sec - in org.jbpm.process.workitem.camel.CamelSqlTest"
          `shouldBe` TestClassInfoLine (TestClassInfo 1 2 3 4 5.694 "org.jbpm.process.workitem.camel.CamelSqlTest")
      it "should parse Maven INFO log lines" $
        parseLine "[INFO] Adding ignore: org.joda.time.*"
          `shouldBe` Maven INFO "Adding ignore: org.joda.time.*"
      it "should parse Maven WARN log lines" $
        parseLine "[WARNING] JAR will be empty - no content was marked for inclusion!"
          `shouldBe` Maven WARNING "JAR will be empty - no content was marked for inclusion!"
      it "should parse Maven ERROR log lines" $
        parseLine "[ERROR] Found 1 illegal transitive type dependencies in artifact 'org.uberfire:uberfire-project-backend:jar:2.11.0.20181022-121005':"
          `shouldBe` Maven ERROR "Found 1 illegal transitive type dependencies in artifact 'org.uberfire:uberfire-project-backend:jar:2.11.0.20181022-121005':"
      it "should parse Maven repo action start lines" $
        parseLine "Repository: droolsjbpm-knowledge"
          `shouldBe` RepoActionStart (GitRepoName "droolsjbpm-knowledge")
  describe "Console.Checks.JunitReport" $
    describe "MethodDuration" $
      it "should parse single MethodDuration JSON object" $
        Aeson.decode @MethodDuration "{\"className\":\"org.dashbuilder.navigation.service.LayoutTemplateAnalyzerTest\",\"duration\":0.002,\"name\":\"testPerspectiveReuseNoRecursiveIssue\"}"
          `shouldBe` (Just $ MethodDuration "org.dashbuilder.navigation.service.LayoutTemplateAnalyzerTest" "testPerspectiveReuseNoRecursiveIssue" 0.002)
  describe "Console.Checks.BuildFinished" $
    describe "getBuildFinishedTimeStamp" $ do
      it "should extract last timestamp from the list of lines" $
        getBuildFinishedTimeStamp
          [ Unknown
          , Maven INFO "Finished at: 2018-10-24T07:00:10-05:00"
          , Maven INFO "Finished at: 2018-10-24T07:48:37-04:00"
          ]
        `shouldBe` Just "2018-10-24 at 07:48:37"
      it "should return Nothing when there are no build timestamps" $
        getBuildFinishedTimeStamp [Unknown] `shouldBe` Nothing
  describe "Console.Checks.RepoDuration" $
    it "should work" $
      getBuildDurationPerRepo
        [ TimedLogLine (et 0) Unknown
        , TimedLogLine (et 1) (RepoActionStart $ GitRepoName "repo1")
        , TimedLogLine (et 11) (Maven INFO "..")
        , TimedLogLine (et 12) (Maven INFO "BUILD SUCCESS")
        , TimedLogLine (et 100) (RepoActionStart $ GitRepoName "repo2")
        , TimedLogLine (et 200) (Maven INFO "..")
        , TimedLogLine (et 250) (Maven INFO "BUILD SUCCESS")
        , TimedLogLine (et 1000) Unknown
        ]
      `shouldBe`
        [ (GitRepoName "repo2", secondsToDuration 100) -- repos sorted by duration descending
        , (GitRepoName "repo1", secondsToDuration 10)
        ]

et :: Double -> ElapsedTime
et = mkElapsedTime
