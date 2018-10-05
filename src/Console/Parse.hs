{-# LANGUAGE OverloadedStrings #-}

module Console.Parse
  ( parsePlain
  , parseTimestamps
  ) where

import Conduit (ConduitT, await, lift, linesUnboundedC, runConduit, sinkList,
                yield, (.|))
import Console.Types
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Util (warn)

parsePlain :: Text -> [LogLine]
parsePlain = fmap LogLine . T.lines

parseTimestamps :: Text -> IO [TimedLogLine]
parseTimestamps logText =
    runConduit $ yield logText
    .| linesUnboundedC
    .| discardLinesWithoutTimestamp
    .| sinkList

discardLinesWithoutTimestamp :: ConduitT Text TimedLogLine IO ()
discardLinesWithoutTimestamp = do
    mLine <- await
    case mLine of
      Nothing -> return ()
      Just line -> do
          case T.words line of
              [] -> lift $ warn "Ignoring empty line"
              perhapsTimestamp:rest -> case readMaybe $ T.unpack perhapsTimestamp of
                  Just t -> yield $ TimedLogLine (mkElapsedTime t) (LogLine $ T.unwords rest)
                  Nothing -> lift . warn $ "Ignoring line without timestamp: " <> line
          discardLinesWithoutTimestamp
