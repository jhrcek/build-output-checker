{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import Console.Checks.TestDuration (MethodDuration (..))
import Console.Parse (parseLine)
import Console.Types
import qualified Data.Aeson as Aeson
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Console.Parse" $ do
    describe "parseLine" $ do
      it "should parse lines where maven download starts" $ do
        parseLine "[INFO] Downloading from mirror-central: http://some.url/1"
          `shouldBe` MavenTransferStart Download (RepoName "mirror-central") (RepoUrl "http://some.url/1")
      it "should parse lines where maven download ends" $ do
        parseLine "[INFO] Downloaded from mirror-central: http://some.url/2 (15 B at 30 MB/s)"
          `shouldBe` MavenTransferEnd Download (RepoName "mirror-central") (RepoUrl "http://some.url/2") (FileSize 15 B) (Just (TransferSpeed 30 MB))
      it "should parse lines where maven upload starts" $ do
        parseLine "[INFO] Uploading to local: http://some.url/3"
          `shouldBe` MavenTransferStart Upload (RepoName "local") (RepoUrl "http://some.url/3")
      it "should parse lines where maven upload ends" $ do
        parseLine "[INFO] Uploaded to local: http://some.url/4 (368 B at 368 kB/s)"
          `shouldBe` MavenTransferEnd Upload (RepoName "local") (RepoUrl "http://some.url/4") (FileSize 368 B) (Just (TransferSpeed 368 KB))
      it "should parse lines where maven upload ends - without transfer speed info" $ do
        parseLine "[INFO] Uploaded to local: http://some.url/5 (1.9 kB)"
          `shouldBe` MavenTransferEnd Upload (RepoName "local") (RepoUrl "http://some.url/5") (FileSize 1.9 KB) Nothing
      it "should parse lines where maven plugin execution starts" $ do
        parseLine "[INFO] --- buildnumber-maven-plugin:1.4:create (get-scm-revision) @ uberfire-project-client ---"
          `shouldBe` MavenPluginExecution (PluginExecution "buildnumber-maven-plugin" "1.4" "create" "get-scm-revision" "uberfire-project-client")
      it "should parse lines with test class info" $ do
        parseLine "Tests run: 1, Failures: 2, Errors: 3, Skipped: 4, Time elapsed: 5.694 sec - in org.jbpm.process.workitem.camel.CamelSqlTest"
          `shouldBe` JunitTestClassSummay (TestClassInfo 1 2 3 4 5.694 "org.jbpm.process.workitem.camel.CamelSqlTest")
  describe "Console.Checks.JunitReport" $ do
      describe "MethodDuration" $ do
        it "should parse single MethodDuration JSON object" $
          Aeson.decode @MethodDuration "{\"className\":\"org.dashbuilder.navigation.service.LayoutTemplateAnalyzerTest\",\"duration\":0.002,\"name\":\"testPerspectiveReuseNoRecursiveIssue\"}"
            `shouldBe` (Just $ MethodDuration "org.dashbuilder.navigation.service.LayoutTemplateAnalyzerTest" "testPerspectiveReuseNoRecursiveIssue" 0.002)
