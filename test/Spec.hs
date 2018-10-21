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
  describe "Console.Parse" $
    describe "parseLine" $ do
      it "should parse lines where maven download starts" $
        parseLine "[INFO] Downloading from mirror-central: http://some.url/1"
          `shouldBe` MavenTransferLine (MavenTransfer Download (RepoName "mirror-central") (RepoUrl "http://some.url/1") TransferStart)
      it "should parse lines where maven download ends" $
        parseLine "[INFO] Downloaded from mirror-central: http://some.url/2 (15 B at 30 MB/s)"
          `shouldBe` MavenTransferLine (MavenTransfer Download (RepoName "mirror-central") (RepoUrl "http://some.url/2") (TransferEnd (FileSize 15 B) (Just (TransferSpeed 30 MB))))
      it "should parse lines where maven upload starts" $
        parseLine "[INFO] Uploading to local: http://some.url/3"
          `shouldBe` MavenTransferLine (MavenTransfer Upload (RepoName "local") (RepoUrl "http://some.url/3") TransferStart)
      it "should parse lines where maven upload ends" $
        parseLine "[INFO] Uploaded to local: http://some.url/4 (368 B at 368 kB/s)"
          `shouldBe` MavenTransferLine (MavenTransfer Upload (RepoName "local") (RepoUrl "http://some.url/4") (TransferEnd (FileSize 368 B) (Just (TransferSpeed 368 KB))))
      it "should parse lines where maven upload ends - without transfer speed info" $
        parseLine "[INFO] Uploaded to local: http://some.url/5 (1.9 kB)"
          `shouldBe` MavenTransferLine (MavenTransfer Upload (RepoName "local") (RepoUrl "http://some.url/5") (TransferEnd (FileSize 1.9 KB) Nothing))
      it "should parse lines where maven plugin execution starts" $
        parseLine "[INFO] --- buildnumber-maven-plugin:1.4:create (get-scm-revision) @ uberfire-project-client ---"
          `shouldBe` PluginExecutionLine (PluginExecution "buildnumber-maven-plugin" "1.4" "create" "get-scm-revision" "uberfire-project-client")
      it "should parse lines with test class info" $
        parseLine "Tests run: 1, Failures: 2, Errors: 3, Skipped: 4, Time elapsed: 5.694 sec - in org.jbpm.process.workitem.camel.CamelSqlTest"
          `shouldBe` TestClassInfoLine (TestClassInfo 1 2 3 4 5.694 "org.jbpm.process.workitem.camel.CamelSqlTest")
  describe "Console.Checks.JunitReport" $
      describe "MethodDuration" $
        it "should parse single MethodDuration JSON object" $
          Aeson.decode @MethodDuration "{\"className\":\"org.dashbuilder.navigation.service.LayoutTemplateAnalyzerTest\",\"duration\":0.002,\"name\":\"testPerspectiveReuseNoRecursiveIssue\"}"
            `shouldBe` (Just $ MethodDuration "org.dashbuilder.navigation.service.LayoutTemplateAnalyzerTest" "testPerspectiveReuseNoRecursiveIssue" 0.002)
