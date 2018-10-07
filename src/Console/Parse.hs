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
import Text.Parsec.Char (anyChar, char, digit)
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
    Left es       -> error $ show es ++ T.unpack txt -- "This shouldn't happen as we're using 'Unknown' to classify not parsed log lines"

logLineP :: Parser LogLine
logLineP =
      mavenDownloadOrUploadP
  <|> pure Unknown
  where
    mavenDownloadOrUploadP =
        (do _ <- try (string "[INFO] Downloading from ")
            (MavenTransferStart Download <$> repoP
                <*> (T.pack <$> anyChar `manyTill` eof))) <|>
        (do _ <- try (string "[INFO] Downloaded from ")
            (MavenTransferEnd Download <$> repoP
                <*> (T.pack <$> anyChar `manyTill` char ' ')
                <*> (char '(' *> fileSizeP)
                <*> (Just <$> (string " at " *> transferSpeedP))))  <|>
        (do _ <- try (string "[INFO] Uploading to ")
            (MavenTransferStart Upload <$> repoP
                <*> (T.pack <$> anyChar `manyTill` eof))) <|>
        (do _ <- try (string "[INFO] Uploaded to ")
            (MavenTransferEnd Upload <$> repoP
                <*> (T.pack <$> anyChar `manyTill` char ' ')
                <*> (char '(' *> fileSizeP)
                <*> (optionMaybe $  -- local uploads only have file size, e.g. "[INFO] Uploaded to local: file://... (1.3 kB)"
                        string " at " *> transferSpeedP)))
        where
          repoP = T.pack <$> anyChar `manyTill` string ": "

fileSizeP :: Parser FileSize
fileSizeP = do
     sz <- doubleP
     _ <- char ' '
     unit <- sizeUnitP
     return $ FileSize sz unit

transferSpeedP :: Parser TransferSpeed
transferSpeedP = do
    spd <- doubleP
    _ <- char ' '
    unit <- sizeUnitP
    _ <- string "/s)"
    return $ TransferSpeed spd unit

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
