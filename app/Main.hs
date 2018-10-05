{-# LANGUAGE OverloadedStrings #-}
module Main where

import Console.Checks.MavenDownload (getMavenDownloadDurations)
import Console.Parse (parseTimestamps)
import Console.Types (getInterval)
import qualified Data.Text.IO as T

main :: IO ()
main = do
    lg <- parseTimestamps =<< T.readFile "consoleText_timestamps"
    print $ getInterval lg
    print . mconcat $ getMavenDownloadDurations lg
