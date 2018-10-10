{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Console.Checks.JunitReport (TestInfo(..), readInfos) where

import Control.Lens (Fold, (^..))
import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:))
import Data.Aeson.Lens (key, _Array, _JSON)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import GHC.Generics (Generic)

data TestInfo = TestInfo
    { tiClass    :: !Text
    , tiMethod   :: !Text
    , tiDuration :: !Double
    } deriving (Eq, Show, Generic, ToJSON)

instance FromJSON TestInfo where
  parseJSON = withObject "TestInfo" $ \o -> do
    tiClass <- o .: "className"
    tiMethod <- o .: "name"
    tiDuration <- o .: "duration"
    return TestInfo{..}

getTestInfos :: ByteString -> [TestInfo]
getTestInfos jsonSource = do
    jsonSource ^.. foldInfos
  where
    foldInfos :: Fold ByteString TestInfo
    foldInfos =
        key "suites" . _Array . traverse .
        key "cases" . _Array . traverse . _JSON

-- TODO get these from endpoint
-- "https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/dailyBuild/job/kieAllBuild-master/lastSuccessfulBuild/testReport/api/json?tree=suites%5Bcases%5BclassName%2Cduration%2Cname%5D%5D"

readInfos :: FilePath -> IO [TestInfo]
readInfos junitReport =
    getTestInfos <$> LBS.readFile junitReport
