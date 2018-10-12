{-# LANGUAGE OverloadedStrings #-}

module Console.Parse
  ( parsePlain
  , parseTimestamps
  , parseLine
  ) where

import Conduit (concatMapMC, linesUnboundedC, runConduit, sinkList, yield, (.|))
import Console.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (double)
import Text.Parsec (parse, string, try, (<|>))
import Text.Parsec.Char (anyChar, char, digit, satisfy)
import Text.Parsec.Combinator (eof, many1, manyTill, option, optionMaybe)
import Text.Parsec.Text (Parser)
import Util (warn)

parsePlain :: Text -> [RawLine]
parsePlain = fmap RawLine . T.lines

parseTimestamps :: Text -> IO [TimedLogLine]
parseTimestamps logText =
    runConduit $ yield logText
    .| linesUnboundedC
    .| concatMapMC parseLineWithTimestamp
    .| sinkList

parseLineWithTimestamp :: Text -> IO (Maybe TimedLogLine)
parseLineWithTimestamp line =
    case double line of
        Right (ts, restOfLine) ->
            let timestamp = mkElapsedTime ts
                logLine = parseLine $ T.stripStart restOfLine
            in pure . Just $ TimedLogLine timestamp logLine
        Left _ -> do
            warn $ "Line without timestamp: " <> line
            pure Nothing

parseLine :: Text -> LogLine
parseLine txt = case parse logLineP "" txt of
    Right logLine -> logLine
    -- "This shouldn't happen as we're using 'Unknown' to classify not parsed log lines"
    Left errors   -> error $ "Failed to parse line '" <> T.unpack txt <> "': " <> show errors

logLineP :: Parser LogLine
logLineP =
      mavenDownloadOrUploadP
  <|> pluginStartP
  <|> junitTestClassSummayP
  <|> pure Unknown
  where
    mavenDownloadOrUploadP =
        (try (string "[INFO] Downloading from ") *>
             (MavenTransferStart Download <$> repoNameP
                <*> repoUrlP eof)) <|>
        (try (string "[INFO] Downloaded from ") *>
             (MavenTransferEnd Download <$> repoNameP
                <*> repoUrlP (string " (")
                <*> fileSizeP
                <*> (Just <$> (string " at " *> transferSpeedP))))  <|>
        (try (string "[INFO] Uploading to ") *>
             (MavenTransferStart Upload <$> repoNameP
                <*> repoUrlP eof)) <|>
        (try (string "[INFO] Uploaded to ") *>
             (MavenTransferEnd Upload <$> repoNameP
                <*> repoUrlP (string " (")
                <*> fileSizeP
                -- local uploads only have file size, e.g. "[INFO] Uploaded to local: file://... (1.3 kB)"
                <*> optionMaybe(string " at " *> transferSpeedP)))

            where
              repoNameP = RepoName . T.pack <$> anyChar `manyTill` string ": "
              repoUrlP :: Parser a -> Parser RepoUrl
              repoUrlP endP = RepoUrl . T.pack <$> anyChar `manyTill` endP

    pluginStartP =
        try (string "[INFO] --- ") *> (MavenPluginExecution <$> pluginExecutionP)

    junitTestClassSummayP =
      try (string "Tests run: " *> (JunitTestClassSummay <$> testClassInfoP))


-- "Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 5.694 sec - in org.jbpm.process.workitem.camel.CamelSqlTest"
testClassInfoP :: Parser TestClassInfo
testClassInfoP = TestClassInfo
    <$> (intP <* string ", Failures: ")
    <*> (intP <* string ", Errors: ")
    <*> (intP <* string ", Skipped: ")
    <*> (intP <* string ", Time elapsed: ")
    <*> (doubleP <* string " sec - in ")
    <*> (T.pack <$> (anyChar `manyTill` eof))

-- Parse "maven-clean-plugin:3.0.0:clean (default-clean) @ uberfire-widgets-properties-editor-backend ---"
pluginExecutionP :: Parser PluginExecution
pluginExecutionP = PluginExecution
    <$> (T.pack <$> many1 nonColon <* char ':')
    <*> (T.pack <$> many1 nonColon <* char ':')
    <*> (T.pack <$> many1 nonSpace <* string " (")
    <*> (T.pack <$> many1 nonRpar <* string ") @ ")
    <*> (T.pack <$> many1 nonSpace <* string " ---")
  where
    nonColon = satisfy (/= ':')
    nonRpar  = satisfy (/= ')')
    nonSpace = satisfy (/= ' ')

fileSizeP :: Parser FileSize
fileSizeP = FileSize
    <$> (doubleP <* char ' ')
    <*> sizeUnitP

transferSpeedP :: Parser TransferSpeed
transferSpeedP = TransferSpeed
    <$> (doubleP <* char ' ')
    <*> (sizeUnitP <* string "/s)")

sizeUnitP :: Parser SizeUnit
sizeUnitP =
    (B  <$ char    'B') <|>
    (KB <$ string "kB") <|>
    (MB <$ string "MB")

doubleP :: Parser Double
doubleP = read <$> ((<>) <$> number <*> decimal)
  where
    number = many1 digit
    decimal = option "" $ (:) <$> char '.' <*> number

intP :: Parser Int
intP = read <$> many1 digit
