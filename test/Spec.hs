{-# LANGUAGE OverloadedStrings #-}
module Main where

import Console.Parse (parseLine)
import Console.Types
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
