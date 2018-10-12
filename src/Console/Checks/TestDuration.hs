{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Console.Checks.TestDuration
    ( MethodDuration(..)
    , readMethodDurations
    , getClassInfos
    ) where

import Console.Types (LogLine (JunitTestClassSummay), TestClassInfo,
                      tciTimeElapsed)
import Control.Lens (Fold, (^..))
import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:))
import Data.Aeson.Lens (key, _Array, _JSON)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data MethodDuration = MethodDuration
    { mdClass    :: !Text
    , mdMethod   :: !Text
    , mdDuration :: !Double
    } deriving (Eq, Show, Generic, ToJSON)

instance FromJSON MethodDuration where
  parseJSON = withObject "MethodDuration" $ \o -> do
    mdClass <- o .: "className"
    mdMethod <- o .: "name"
    mdDuration <- o .: "duration"
    return MethodDuration{..}

getMethodDurations :: ByteString -> [MethodDuration]
getMethodDurations jsonSource =
    jsonSource ^.. foldInfos
  where
    foldInfos :: Fold ByteString MethodDuration
    foldInfos =
        key "suites" . _Array . traverse .
        key "cases" . _Array . traverse . _JSON

-- TODO get these from endpoint
-- "https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/dailyBuild/job/kieAllBuild-master/lastSuccessfulBuild/testReport/api/json?tree=suites%5Bcases%5BclassName%2Cduration%2Cname%5D%5D"

readMethodDurations :: FilePath -> IO [MethodDuration]
readMethodDurations junitReport =
    sortLongestFirst . getMethodDurations <$> LBS.readFile junitReport
  where
    sortLongestFirst :: [MethodDuration] -> [MethodDuration]
    sortLongestFirst = sortOn (Down . mdDuration)

getClassInfos :: [LogLine] -> [TestClassInfo]
getClassInfos = sortClasses . mapMaybe getClassInfo
  where
    getClassInfo :: LogLine -> Maybe TestClassInfo
    getClassInfo (JunitTestClassSummay tci) = Just tci
    getClassInfo _                          = Nothing

    sortClasses :: [TestClassInfo] -> [TestClassInfo]
    sortClasses = sortOn (Down . tciTimeElapsed)
