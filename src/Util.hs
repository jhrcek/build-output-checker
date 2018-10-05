{-# LANGUAGE OverloadedStrings #-}
module Util (warn) where

import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import System.IO (stderr)

warn :: Text -> IO ()
warn = hPutStrLn stderr . ("[WARNING] " <>)
